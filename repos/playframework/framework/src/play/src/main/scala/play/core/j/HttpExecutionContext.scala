/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.core.j

import java.util.concurrent.Executor

import play.mvc.Http
import scala.compat.java8.FutureConverters
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object HttpExecutionContext {

  /**
    * Create an HttpExecutionContext with values from the current thread.
    */
  def fromThread(delegate: ExecutionContext): ExecutionContextExecutor =
    new HttpExecutionContext(Thread.currentThread().getContextClassLoader(),
                             Http.Context.current.get(),
                             delegate)

  /**
    * Create an HttpExecutionContext with values from the current thread.
    *
    * This method is necessary to prevent ambiguous method compile errors since ExecutionContextExecutor
    */
  def fromThread(
      delegate: ExecutionContextExecutor): ExecutionContextExecutor =
    fromThread(delegate: ExecutionContext)

  /**
    * Create an HttpExecutionContext with values from the current thread.
    */
  def fromThread(delegate: Executor): ExecutionContextExecutor =
    new HttpExecutionContext(Thread.currentThread().getContextClassLoader(),
                             Http.Context.current.get(),
                             FutureConverters.fromExecutor(delegate))

  /**
    * Create an ExecutionContext that will, when prepared, be created with values from that thread.
    */
  def unprepared(delegate: ExecutionContext) = new ExecutionContext {
    def execute(runnable: Runnable) =
      delegate
        .execute(runnable) // FIXME: Make calling this an error once SI-7383 is fixed
    def reportFailure(t: Throwable) = delegate.reportFailure(t)
    override def prepare(): ExecutionContext = fromThread(delegate)
  }
}

/**
  * Manages execution to ensure that the given context ClassLoader and Http.Context are set correctly
  * in the current thread. Actual execution is performed by a delegate ExecutionContext.
  */
class HttpExecutionContext(contextClassLoader: ClassLoader,
                           httpContext: Http.Context,
                           delegate: ExecutionContext)
    extends ExecutionContextExecutor {
  override def execute(runnable: Runnable) =
    delegate.execute(new Runnable {
      def run() {
        val thread = Thread.currentThread()
        val oldContextClassLoader = thread.getContextClassLoader()
        val oldHttpContext = Http.Context.current.get()
        thread.setContextClassLoader(contextClassLoader)
        Http.Context.current.set(httpContext)
        try {
          runnable.run()
        } finally {
          thread.setContextClassLoader(oldContextClassLoader)
          Http.Context.current.set(oldHttpContext)
        }
      }
    })

  override def reportFailure(t: Throwable) = delegate.reportFailure(t)

  override def prepare(): ExecutionContext = {
    val delegatePrepared = delegate.prepare()
    if (delegatePrepared eq delegate) {
      this
    } else {
      new HttpExecutionContext(contextClassLoader,
                               httpContext,
                               delegatePrepared)
    }
  }
}
