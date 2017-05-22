/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.streams.impl

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.duration.{FiniteDuration, SECONDS, MILLISECONDS}

/**
  * Utility for recording events in a queue. Useful for
  * checking the ordering properties of asynchronous code. Code that does
  * stuff records an event, then the test can check that events occur in
  * the right order and at the right time.
  */
class EventRecorder(nextTimeout: FiniteDuration = FiniteDuration(20, SECONDS),
                    isEmptyDelay: FiniteDuration = FiniteDuration(
                          200, MILLISECONDS))

  private val events = new LinkedBlockingQueue[AnyRef]

  /** Record an event. */
  def record(e: AnyRef) = events.add(e)

  /** Pull the next event, waiting up to `nextTimeout`. */
  def next(): AnyRef =
    events.poll(nextTimeout.length, nextTimeout.unit)

  /** Wait for `isEmptyDelay` then check if the event queue is empty. */
  def isEmptyAfterDelay(waitMillis: Long = 50): Boolean =
    Thread.sleep(isEmptyDelay.toMillis)
    events.isEmpty
