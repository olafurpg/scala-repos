package com.twitter.util

import java.util.concurrent.atomic.{AtomicLong, AtomicReference, AtomicReferenceArray}
import java.util.{List => JList}
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.mutable.Buffer
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Trait Var represents a variable. It is a reference cell which is
  * composable: dependent Vars (derived through flatMap) are
  * recomputed automatically when independent variables change -- they
  * implement a form of self-adjusting computation.
  *
  * Vars are observed, notifying users whenever the variable changes.
  *
  * @note Vars do not always perform the minimum amount of
  * re-computation.
  *
  * @note There are no well-defined error semantics for Var. Vars are
  * computed lazily, and the updating thread will receive any
  * exceptions thrown while computing derived Vars.
  *
  * Note: There is a Java-friendly API for this trait: [[com.twitter.util.AbstractVar]].
  */
trait Var[+T]  self =>
  import Var.Observer

  /**
    * Observe this Var. `f` is invoked each time the variable changes,
    * and synchronously with the first call to this method.
    */
  private[util] final def observe(f: T => Unit): Closable =
    observe(0, Observer(f))

  /**
    * Concrete implementations of Var implement observe. This is
    * called for each toplevel observe. Depths indicate the relative
    * structural depth of the observation, from the frame of reference
    * of the root call to observe. (Each Var derived via flatMap
    * increases the depth.) Depths are used to order the invocation of
    * update callbacks. This is used to ensure that updates proceed in
    * topological order so that every input variable is fully resolved
    * before recomputing a derived variable.
    */
  protected def observe(depth: Int, obs: Observer[T]): Closable

  /**
    * Create a derived variable by applying `f` to the contained
    * value.
    */
  def map[U](f: T => U): Var[U] = flatMap(t => Var.value(f(t)))

  /**
    * Create a dependent Var which behaves as `f` applied to the
    * current value of this Var. FlatMap manages a dynamic dependency
    * graph: the dependent Var is detached and recomputed  whenever
    * the outer Var changes, but only if there are any observers.  An
    * unobserved Var returned by flatMap will not invoke `f`
    */
  def flatMap[U](f: T => Var[U]): Var[U] = new Var[U]
    def observe(depth: Int, obs: Observer[U]) =
      val inner = new AtomicReference(Closable.nop)
      val outer = self.observe(
          depth,
          Observer(
              t =>
              // TODO: Right now we rely on synchronous propagation; and
              // thus also synchronous closes. We should instead perform
              // asynchronous propagation so that it is is safe &
              // predictable to have asynchronously closing Vars, for
              // example. Currently the only source of potentially
              // asynchronous closing is Var.async; here we have modified
              // the external process to close asynchronously with the Var
              // itself. Thus we know the code path here is synchronous:
              // we control all Var implementations, and also all Closable
              // combinators have been modified to evaluate their respective
              // Futures eagerly.
              val done = inner.getAndSet(f(t).observe(depth + 1, obs)).close()
              assert(done.isDone)
          ))

      Closable.sequence(outer, Closable.ref(inner))

  def join[U](other: Var[U]): Var[(T, U)] =
    for { t <- self; u <- other } yield (t, u)

  /**
    * An Event where changes in Var are emitted. The current value
    * of this Var is emitted synchronously upon subscription.
    *
    * All changes to this Var are guaranteed to be published to the
    * Event.
    */
  lazy val changes: Event[T] = new Event[T]
    def register(s: Witness[T]) =
      self.observe  newv =>
        s.notify(newv)

  /**
    * Produce an [[Event]] reflecting the differences between
    * each update to this [[Var]].
    */
  def diff[CC[_]: Diffable, U](
      implicit toCC: T <:< CC[U]): Event[Diff[CC, U]] =
    changes.diff

  def sample(): T = Var.sample(this)

/**
  * Abstract `Var` class for Java compatibility.
  */
abstract class AbstractVar[T] extends Var[T]

/**
  * Note: There is a Java-friendly API for this object: [[com.twitter.util.Vars]].
  */
object Var

  /**
    * A Var observer. Observers are owned by exactly one producer,
    * enforced by a leasing mechanism.
    */
  private[util] class Observer[-T](observe: T => Unit)
    private[this] var thisOwner: AnyRef = null
    private[this] var thisVersion = Long.MinValue

    /**
      * Claim this observer with owner `newOwner`. Claiming
      * an observer gives the owner exclusive rights to publish
      * to it while it has not been claimed by another owner.
      */
    def claim(newOwner: AnyRef): Unit = synchronized
      if (thisOwner ne newOwner)
        thisOwner = newOwner
        thisVersion = Long.MinValue

    /**
      * Publish the given versioned value with the given owner.
      * If the owner is not current (because another has claimed
      * the observer), or if the version has already published (by
      * assumption of a monotonically increasing version number)
      * the publish operation is a no-op.
      */
    def publish(owner: AnyRef, value: T, version: Long): Unit = synchronized
      if ((owner eq thisOwner) && thisVersion < version)
        thisVersion = version
        observe(value)

  private[util] object Observer
    def apply[T](k: T => Unit) = new Observer(k)

  /**
    * Sample the current value of this Var. Note that this may lead to
    * surprising results for lazily defined Vars: the act of observing
    * a Var may be kick off a process to populate it; the value
    * returned from sample may then reflect an intermediate value.
    */
  def sample[T](v: Var[T]): T =
    var opt: Option[T] = None
    v.observe(0, Observer(v => opt = Some(v))).close()
    opt.get

  object Sampled
    def apply[T](v: T): Var[T] = value(v)
    def unapply[T](v: Var[T]): Option[T] = Some(sample(v))

  /**
    * Create a new, updatable Var with an initial value. We call
    * such Vars independent -- derived Vars being dependent
    * on these.
    */
  def apply[T](init: T): Var[T] with Updatable[T] with Extractable[T] =
    new UpdatableVar(init)

  /**
    * Constructs a Var from an initial value plus an event stream of
    * changes. Note that this eagerly subscribes to the event stream;
    * it is unsubscribed whenever the returned Var is collected.
    */
  def apply[T](init: T, e: Event[T]): Var[T] =
    val v = Var(init)
    Closable.closeOnCollect(e.register(Witness(v)), v)
    v

  /**
    * Patch reconstructs a [[Var]] based on observing the incremental
    * changes presented in the underlying [[Diff]]s.
    *
    * Note that this eagerly subscribes to the event stream;
    * it is unsubscribed whenever the returned Var is collected.
    */
  def patch[CC[_]: Diffable, T](diffs: Event[Diff[CC, T]]): Var[CC[T]] =
    val v = Var(Diffable.empty[CC, T]: CC[T])
    Closable.closeOnCollect(diffs.respond  diff =>
      synchronized
        v() = diff.patch(v())
    , v)
    v

  private case class Value[T](v: T) extends Var[T]
    protected def observe(depth: Int, obs: Observer[T]): Closable =
      obs.claim(this)
      obs.publish(this, v, 0)
      Closable.nop

  /**
    * Create a new, constant, v-valued Var.
    */
  def value[T](v: T): Var[T] = Value(v)

  /**
    * Collect a collection of Vars into a Var of collection.
    */
  def collect[T : ClassTag, CC[X] <: Traversable[X]](vars: CC[Var[T]])(
      implicit newBuilder: CanBuildFrom[CC[T], T, CC[T]]): Var[CC[T]] =
    val vs = vars.toArray

    def tree(begin: Int, end: Int): Var[Seq[T]] =
      if (begin == end) Var(Seq.empty)
      else if (begin == end - 1) vs(begin).map(t => Seq(t))
      else
        val n = (end - begin) / 2

        for
          left <- tree(begin, begin + n)
          right <- tree(begin + n, end)
        yield left ++ right

    tree(0, vs.length).map  ts =>
      val b = newBuilder()
      b ++= ts
      b.result()

  /**
    * Collect a List of Vars into a new Var of List.
    *
    * @param vars a java.util.List of Vars
    * @return a Var[java.util.List[A]] containing the collected values from vars.
    */
  def collect[T <: Object](vars: JList[Var[T]]): Var[JList[T]] =
    // we cast to Object and back because we need a ClassTag[T]
    val list = vars.asScala.asInstanceOf[Buffer[Var[Object]]]
    collect(list).map(_.asJava).asInstanceOf[Var[JList[T]]]

  private object create
    sealed trait State[+T]
    object Idle extends State[Nothing]
    case class Observing[T](n: Int, v: Var[T], c: Closable) extends State[T]

  /**
    * Create a new Var whose values are provided asynchronously by
    * `update`. The returned Var is dormant until it is observed:
    * `update` is called by-need. Such observations are also reference
    * counted so that simultaneous observations do not result in
    * multiple invocations of `update`. When the last observer stops
    * observing, the [[com.twitter.util.Closable]] returned
    * from `update` is closed. Subsequent observations result in a new
    * call to `update`.
    *
    * `empty` is used to fill the returned Var until `update` has
    * provided a value. The first observation of the returned Var is
    * synchronous with the call to `update`--it is guaranteed the the
    * opportunity to fill the Var before the observer sees any value
    * at all.
    *
    * Updates from `update` are ignored after the returned
    * [[com.twitter.util.Closable]] is closed.
    */
  def async[T](empty: T)(update: Updatable[T] => Closable): Var[T] =
    new Var[T]
      import create._
      private var state: State[T] = Idle

      private val closable = Closable.make  deadline =>
        synchronized
          state match
            case Idle =>
              Future.Done
            case Observing(1, _, c) =>
              state = Idle
              // We close the external process asynchronously from the
              // async Var so that it is safe to Await Var.close() in
              // flatMap. (See the TODO there.)
              c.close(deadline)
              Future.Done
            case Observing(n, v, c) =>
              state = Observing(n - 1, v, c)
              Future.Done

      protected def observe(depth: Int, obs: Observer[T]): Closable =
        val v = synchronized
          state match
            case Idle =>
              val v = Var(empty)
              val c = update(v)
              state = Observing(1, v, c)
              v
            case Observing(n, v, c) =>
              state = Observing(n + 1, v, c)
              v

        val c = v.observe(depth, obs)
        Closable.sequence(c, closable)

private object UpdatableVar
  import Var.Observer

  case class Party[T](obs: Observer[T], depth: Int, n: Long)
    @volatile var active = true

  case class State[T](
      value: T, version: Long, parties: immutable.SortedSet[Party[T]])
    def -(p: Party[T]) = copy(parties = parties - p)
    def +(p: Party[T]) = copy(parties = parties + p)
    def :=(newv: T) = copy(value = newv, version = version + 1)

  implicit def order[T] = new Ordering[Party[T]]
    // This is safe because observers are compared
    // only from the same counter.
    def compare(a: Party[T], b: Party[T]): Int =
      val c1 = a.depth compare b.depth
      if (c1 != 0) return c1
      a.n compare b.n

private[util] class UpdatableVar[T](init: T)
    extends Var[T] with Updatable[T] with Extractable[T]
  import UpdatableVar._
  import Var.Observer

  private[this] val n = new AtomicLong(0)
  private[this] val state = new AtomicReference(
      State[T](init, 0, immutable.SortedSet.empty))

  @tailrec
  private[this] def cas(next: State[T] => State[T]): State[T] =
    val from = state.get
    val to = next(from)
    if (state.compareAndSet(from, to)) to else cas(next)

  def apply(): T = state.get.value

  def update(newv: T): Unit = synchronized
    val State(value, version, parties) = cas(_ := newv)
    for (p @ Party(obs, _, _) <- parties)
      // An antecedent update may have closed the current
      // party (e.g. flatMap does this); we need to check that
      // the party is active here in order to prevent stale updates.
      if (p.active) obs.publish(this, value, version)

  protected def observe(depth: Int, obs: Observer[T]): Closable =
    obs.claim(this)
    val party = Party(obs, depth, n.getAndIncrement())
    val State(value, version, _) = cas(_ + party)
    obs.publish(this, value, version)

    new Closable
      def close(deadline: Time) =
        party.active = false
        cas(_ - party)
        Future.Done

  override def toString = "Var(" + state.get.value + ")@" + hashCode

/**
  * Java adaptation of `Var[T] with Updatable[T] with Extractable[T]`.
  */
class ReadWriteVar[T](init: T) extends UpdatableVar[T](init)
