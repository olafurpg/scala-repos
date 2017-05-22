/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.iteratee

import scala.concurrent.ExecutionContext

object TestExecutionContext

  /**
    * Create a `TestExecutionContext` that delegates to the iteratee package's default `ExecutionContext`.
    */
  def apply(): TestExecutionContext =
    new TestExecutionContext(Execution.trampoline)

/**
  * An `ExecutionContext` that counts its executions.
  *
  * @param delegate The underlying `ExecutionContext` to delegate execution to.
  */
class TestExecutionContext(delegate: ExecutionContext)
    extends ExecutionContext  top =>

  val count = new java.util.concurrent.atomic.AtomicInteger()

  val local = new ThreadLocal[java.lang.Boolean]

  def preparable[A](body: => A): A =
    local.set(true)
    try body finally local.set(null)

  def execute(runnable: Runnable): Unit =
    throw new RuntimeException(
        "Cannot execute unprepared TestExecutionContext")

  def reportFailure(t: Throwable): Unit =
    println(t)

  override def prepare(): ExecutionContext =
    val isLocal = Option(local.get()).getOrElse(false: java.lang.Boolean)
    if (!isLocal)
      throw new RuntimeException(
          "Can only prepare TestExecutionContext within 'preparable' scope")
    val preparedDelegate = delegate.prepare()
    return new ExecutionContext

      def execute(runnable: Runnable): Unit =
        count.getAndIncrement()
        preparedDelegate.execute(runnable)

      def reportFailure(t: Throwable): Unit =
        println(t)

  def executionCount: Int = count.get()
