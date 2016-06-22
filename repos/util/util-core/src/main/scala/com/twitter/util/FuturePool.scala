package com.twitter.util

import com.twitter.concurrent.NamedPoolThreadFactory
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{CancellationException, ExecutionException, ExecutorService, Executors, RejectedExecutionException}
import scala.runtime.NonLocalReturnControl

/**
  * A FuturePool executes tasks asynchronously, typically using a pool
  * of worker threads.
  */
trait FuturePool {
  def apply[T](f: => T): Future[T]
}

/**
  * Note: There is a Java-friendly API for this object: [[com.twitter.util.FuturePools]].
  */
object FuturePool {

  /**
    * Creates a [[FuturePool]] backed by an `java.util.concurrent.ExecutorService`.
    */
  def apply(executor: ExecutorService): ExecutorServiceFuturePool =
    new ExecutorServiceFuturePool(executor)

  /**
    * Creates a [[FuturePool]] backed by an `java.util.concurrent.ExecutorService`
    * which propagates cancellation.
    */
  def interruptible(executor: ExecutorService): ExecutorServiceFuturePool =
    new InterruptibleExecutorServiceFuturePool(executor)

  /**
    * A [[FuturePool]] that really isn't; it executes tasks immediately
    * without waiting.  This can be useful in unit tests.
    */
  val immediatePool: FuturePool = new FuturePool {
    def apply[T](f: => T): Future[T] = Future(f)
  }

  private lazy val defaultExecutor = Executors.newCachedThreadPool(
      new NamedPoolThreadFactory("UnboundedFuturePool", makeDaemons = true)
  )

  /**
    * The default future pool, using a cached threadpool, provided by
    * [[java.util.concurrent.Executors.newCachedThreadPool]]. Note
    * that this is intended for IO concurrency; computational
    * parallelism typically requires special treatment. If an interrupt
    * is raised on a returned Future and the work has started, the worker
    * thread will not be interrupted.
    */
  lazy val unboundedPool: FuturePool = new ExecutorServiceFuturePool(
      defaultExecutor)

  /**
    * The default future pool, using a cached threadpool, provided by
    * [[java.util.concurrent.Executors.newCachedThreadPool]]. Note
    * that this is intended for IO concurrency; computational
    * parallelism typically requires special treatment.  If an interrupt
    * is raised on a returned Future and the work has started, an attempt
    * will will be made to interrupt the worker thread.
    */
  lazy val interruptibleUnboundedPool: FuturePool =
    new InterruptibleExecutorServiceFuturePool(defaultExecutor)
}

/**
  * A [[FuturePool]] backed by a `java.util.concurrent.ExecutorService`
  * that supports cancellation.
  */
class InterruptibleExecutorServiceFuturePool(executor: ExecutorService)
    extends ExecutorServiceFuturePool(executor, true)

/**
  * A [[FuturePool]] implementation backed by an `java.util.concurrent.ExecutorService`.
  *
  * If a piece of work has started, it cannot be cancelled and will not propagate
  * cancellation unless `interruptible` is true. If you want to propagate cancellation,
  * use an [[InterruptibleExecutorServiceFuturePool]].
  */
class ExecutorServiceFuturePool protected[this] (val executor: ExecutorService,
                                                 val interruptible: Boolean)
    extends FuturePool {
  def this(executor: ExecutorService) = this(executor, false)

  def apply[T](f: => T): Future[T] = {
    val runOk = new AtomicBoolean(true)
    val p = new Promise[T]
    val task = new Runnable {
      private[this] val saved = Local.save()

      def run(): Unit = {
        // Make an effort to skip work in the case the promise
        // has been cancelled or already defined.
        if (!runOk.compareAndSet(true, false)) return

        val current = Local.save()
        Local.restore(saved)

        try p.updateIfEmpty(Try(f)) catch {
          case nlrc: NonLocalReturnControl[_] =>
            val fnlrc = new FutureNonLocalReturnControl(nlrc)
            p.updateIfEmpty(Throw(fnlrc))
            throw fnlrc
          case e: Throwable =>
            p.updateIfEmpty(Throw(new ExecutionException(e)))
            throw e
        } finally Local.restore(current)
      }
    }

    // This is safe: the only thing that can call task.run() is
    // executor, the only thing that can raise an interrupt is the
    // receiver of this value, which will then be fully initialized.
    val javaFuture = try executor.submit(task) catch {
      case e: RejectedExecutionException =>
        runOk.set(false)
        p.setException(e)
        null
    }

    p.setInterruptHandler {
      case cause =>
        if (interruptible || runOk.compareAndSet(true, false)) {
          val exc = new CancellationException
          exc.initCause(cause)
          if (p.updateIfEmpty(Throw(exc))) javaFuture.cancel(true)
        }
    }

    p
  }
}
