package spire
package examples

import spire.algebra._
import spire.math.{Natural, UInt}

import scala.collection.mutable

object SetUtil
  def powerStream[A](members: Stream[A]): Stream[Set[A]] =
    val done = Stream.empty[Set[A]]

    def powerLoop(as: Stream[A], i: Int): Stream[Set[A]] =
      def nthLoop(as: Stream[A], i: Int): Stream[Set[A]] = as match
        case a #:: tail =>
          if (i == 0)
            Set(a) #:: done
          else
            val next = nthLoop(tail, i - 1)
            next #::: next.map(_ + a)
        case _ =>
          done
      val nth = nthLoop(as, i)
      if (nth.isEmpty) nth else nth #::: powerLoop(as, i + 1)

    Set.empty[A] #:: powerLoop(members, 0)

object PureSet
  def empty[A]: PureSet[A] = PureSet[A](a => false)
  def infinite[A]: PureSet[A] = PureSet[A](a => true)

  implicit def monoid[A] = new Monoid[PureSet[A]]
    def id: PureSet[A] = empty
    def op(x: PureSet[A], y: PureSet[A]): PureSet[A] = x | y

  implicit def bool[A] = new Bool[PureSet[A]]
    def one: PureSet[A] = infinite
    def zero: PureSet[A] = empty
    def complement(a: PureSet[A]): PureSet[A] = ~a
    def and(a: PureSet[A], b: PureSet[A]): PureSet[A] = a & b
    def or(a: PureSet[A], b: PureSet[A]): PureSet[A] = a | b
    override def xor(a: PureSet[A], b: PureSet[A]): PureSet[A] = a ^ b

case class PureSet[A](f: A => Boolean) extends Function1[A, Boolean]  lhs =>
  def apply(a: A): Boolean =
    f(a)
  def toSet(universe: Set[A]): Set[A] =
    universe.filter(f)
  def toMathSet(universe: Set[A]): MathSet[A] =
    MathSet(universe.filter(f))
  def filter(g: A => Boolean): PureSet[A] =
    PureSet(a => f(a) && g(a))
  def contramap[B](g: B => A): PureSet[B] =
    PureSet(b => f(g(b)))

  def unary_~(): PureSet[A] =
    PureSet(a => !(f(a)))
  def |(rhs: PureSet[A]): PureSet[A] =
    PureSet(a => lhs.f(a) || rhs.f(a))
  def &(rhs: PureSet[A]): PureSet[A] =
    PureSet(a => lhs.f(a) && rhs.f(a))
  def --(rhs: PureSet[A]): PureSet[A] =
    PureSet(a => lhs.f(a) && !rhs.f(a))
  def ^(rhs: PureSet[A]): PureSet[A] =
    PureSet(a => lhs.f(a) ^ rhs.f(a))

  def cross[B](rhs: PureSet[B]): PureSet[(A, B)] =
    PureSet[(A, B)](t => lhs.f(t._1) && rhs.f(t._2))

  def power(universe: Stream[A]): Stream[Set[A]] =
    SetUtil.powerStream(universe.filter(f))

object MathSet
  def empty[A]: MathSet[A] = Fin(Set.empty)
  def apply[A](as: A*): MathSet[A] = Fin(as.toSet)
  def infinite[A]: MathSet[A] = Inf(Set.empty)
  def apply[A](as: Set[A]): MathSet[A] = Fin(as)

  case class Fin[A](members: Set[A]) extends MathSet[A]  lhs =>
    def apply(a: A): Boolean =
      members(a)
    def toSet(universe: Set[A]): Set[A] =
      universe & members
    def toPureSet: PureSet[A] =
      PureSet(members)
    def filter(f: A => Boolean): Fin[A] =
      Fin(members.filter(f))
    def cross[B](rhs: Fin[B]): Fin[(A, B)] =
      Fin(lhs.members.flatMap(a => rhs.members.map(b => (a, b))))
    def size(usize: Option[Natural]): Option[Natural] =
      Some(Natural(members.size))

    def map[B](f: A => B): MathSet[B] = Fin(members.map(f))
    def unary_~(): MathSet[A] = Inf(members)

    override def toString: String =
      members.mkString("{", ", ", "}")

  case class Inf[A](outsiders: Set[A]) extends MathSet[A]
    def apply(a: A): Boolean =
      !outsiders(a)
    def toSet(universe: Set[A]): Set[A] =
      universe -- outsiders
    def toPureSet: PureSet[A] =
      PureSet(a => !outsiders(a))
    def size(usize: Option[Natural]): Option[Natural] =
      usize.map(_ - UInt(outsiders.size))

    def map[B](f: A => B): MathSet[B] = Inf(outsiders.map(f))
    def unary_~(): MathSet[A] = Fin(outsiders)

    override def toString: String =
      if (outsiders.isEmpty) "(U)"
      else outsiders.mkString("(U -- {", ", ", "})")

  implicit def monoid[A] = new Monoid[MathSet[A]]
    def id: MathSet[A] = empty
    def op(x: MathSet[A], y: MathSet[A]): MathSet[A] = x | y

  implicit def bool[A] = new Bool[MathSet[A]]
    def one: MathSet[A] = infinite
    def zero: MathSet[A] = empty
    def complement(a: MathSet[A]): MathSet[A] = ~a
    def and(a: MathSet[A], b: MathSet[A]): MathSet[A] = a & b
    def or(a: MathSet[A], b: MathSet[A]): MathSet[A] = a | b
    override def xor(a: MathSet[A], b: MathSet[A]): MathSet[A] = a ^ b

sealed trait MathSet[A] extends Function1[A, Boolean]  lhs =>
  import MathSet._

  def toSet(universe: Set[A]): Set[A]
  def toPureSet: PureSet[A]
  def size(usize: Option[Natural]): Option[Natural]
  def toFinite(universe: Set[A]): Fin[A] = Fin(toSet(universe))

  def map[B](f: A => B): MathSet[B]
  def unary_~(): MathSet[A]

  def |(rhs: MathSet[A]): MathSet[A] = (lhs, rhs) match
    case (Fin(x), Fin(y)) => Fin(x | y)
    case (Fin(x), Inf(y)) => Inf(y -- x)
    case (Inf(x), Fin(y)) => Inf(x -- y)
    case (Inf(x), Inf(y)) => Inf(x & y)

  def &(rhs: MathSet[A]): MathSet[A] = (lhs, rhs) match
    case (Fin(x), Fin(y)) => Fin(x & y)
    case (Fin(x), Inf(y)) => Fin(x -- y)
    case (Inf(x), Fin(y)) => Fin(y -- x)
    case (Inf(x), Inf(y)) => Inf(x | y)

  def --(rhs: MathSet[A]): MathSet[A] = (lhs, rhs) match
    case (Fin(x), Fin(y)) => Fin(x -- y)
    case (Fin(x), Inf(y)) => Fin(x & y)
    case (Inf(x), Fin(y)) => Inf(x | y)
    case (Inf(x), Inf(y)) => Fin(y -- x)

  private def xor(x: Set[A], y: Set[A]): Set[A] =
    val builder = new mutable.SetBuilder[A, Set[A]](Set.empty)
    x.foreach(a => if (!y(a)) builder += a)
    y.foreach(a => if (!x(a)) builder += a)
    builder.result()

  def ^(rhs: MathSet[A]): MathSet[A] = (lhs, rhs) match
    case (Fin(x), Fin(y)) => Fin(xor(x, y))
    case (Fin(x), Inf(y)) => Inf(x -- y)
    case (Inf(x), Fin(y)) => Inf(y -- x)
    case (Inf(x), Inf(y)) => Fin(xor(x, y))

  def power(universe: Stream[A]): Stream[Set[A]] =
    SetUtil.powerStream(universe.filter(this))
