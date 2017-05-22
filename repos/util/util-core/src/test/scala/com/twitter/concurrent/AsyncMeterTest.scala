package com.twitter.concurrent

import com.twitter.util._
import com.twitter.conversions.time._
import java.util.concurrent.{RejectedExecutionException, CancellationException}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AsyncMeterTest extends FunSuite
  test("AsyncMeter shouldn't wait at all when there aren't any waiters.")
    val timer = new MockTimer
    val meter = new AsyncMeter(1, 1.second, 100)(timer)
    val result = meter.await(1)
    assert(result.isDone)

  test(
      "AsyncMeter should allow more than one waiter and allow them on the schedule.")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(1, 1.second, 100)(timer)
      val ready = meter.await(1)
      val waiter = meter.await(1)
      assert(ready.isDone)
      assert(!waiter.isDefined)

      ctl.advance(1.second)
      timer.tick()
      assert(waiter.isDone)

  test(
      "AsyncMeter shouldn't allow a waiter until interval has passed since the last allowance.")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(1, 1.second, 100)(timer)
      val ready = meter.await(1)
      assert(ready.isDone)

      val waiter = meter.await(1)
      assert(!waiter.isDefined)

      timer.tick()
      assert(!waiter.isDefined)

      ctl.advance(1.second)
      timer.tick()
      assert(waiter.isDone)

  test(
      "AsyncMeter should fail waiters that wait over the limit, but still allow the rest")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(1, 1.second, 1)(timer)
      val ready = meter.await(1)
      assert(ready.isDone)

      val waiter = meter.await(1)
      val rejected = meter.await(1)
      assert(!waiter.isDefined)
      assert(rejected.isDefined)
      intercept[RejectedExecutionException]
        Await.result(rejected, 5.seconds)

      ctl.advance(1.second)
      timer.tick()
      waiter.isDone

  test(
      "AsyncMeter should allow a waiter to be removed from the queue on interruption.")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(1, 1.second, 100)(timer)
      var nr = 0
      val ready = meter.await(1)
      val waiter = meter.await(1)
      assert(ready.isDone)
      assert(!waiter.isDefined)
      val e = new Exception("boom!")

      waiter.raise(e)
      assert(waiter.isDefined)
      val actual = intercept[CancellationException]
        Await.result(waiter, 5.seconds)
      assert(actual.getCause == e)

  test("AsyncMeter should allow more than one waiter in a ready period")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(2, 1.second, 100)(timer)
      val ready = meter.await(2)
      assert(ready.isDone)

      val first = meter.await(1)
      val second = meter.await(1)
      assert(!first.isDefined)
      assert(!second.isDefined)

      ctl.advance(1.second)
      timer.tick()
      assert(first.isDone)
      assert(second.isDone)

  test("AsyncMeter should allow an expensive call to be satisfied slowly")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(2, 1.second, 100)(timer)
      val ready = meter.await(2)
      assert(ready.isDone)

      val waiter = meter.await(2)
      assert(!waiter.isDefined)

      ctl.advance(500.milliseconds)
      timer.tick()
      assert(!waiter.isDefined)

      ctl.advance(500.milliseconds)
      timer.tick()
      assert(waiter.isDone)

  test("AsyncMeter should reject greedy awaiters")
    val timer = new MockTimer
    val meter = new AsyncMeter(2, 1.second, 100)(timer)
    val greedy = meter.await(3)
    assert(greedy.isDefined)
    intercept[IllegalArgumentException]
      Await.result(greedy, 5.seconds)

  test("AsyncMeter should not allow small queue jumpers")
    val timer = new MockTimer
    val meter = new AsyncMeter(6, 1.second, 100)(timer)
    val ready = meter.await(3)
    val first = meter.await(4)
    val second = meter.await(4)
    assert(ready.isDone)
    assert(!first.isDefined)
    assert(!second.isDefined)

  test("AsyncMeter should allow parts of tokens")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(3, 2.millisecond, 100)(timer)
      val ready = meter.await(3)
      val first = meter.await(1)
      val second = meter.await(1)
      val third = meter.await(1)
      assert(ready.isDone)
      ctl.advance(1.millisecond)
      timer.tick()
      assert(first.isDone)
      assert(!second.isDefined)
      assert(!third.isDefined)
      ctl.advance(1.millisecond)
      timer.tick()
      assert(second.isDone)
      assert(third.isDone)

  test("AsyncMeter.extraWideAwait should handle big awaits")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(6, 1.second, 100)(timer)
      val greedy = AsyncMeter.extraWideAwait(12, meter)
      assert(!greedy.isDefined)
      ctl.advance(1.second)
      timer.tick()
      assert(greedy.isDone)

  test("AsyncMeter.extraWideAwait shouldn't block after being rejected")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(6, 1.second, 2)(timer)
      val greedy = AsyncMeter.extraWideAwait(24, meter)
      val first = meter.await(6)
      assert(greedy.isDefined)
      assert(!first.isDefined)
      intercept[RejectedExecutionException]
        Await.result(greedy, 5.seconds)
      ctl.advance(1.second)
      timer.tick()
      assert(first.isDone)

  test("AsyncMeter.extraWideAwait shouldn't block after being interrupted")
    val timer = new MockTimer
    Time.withCurrentTimeFrozen  ctl =>
      val meter = new AsyncMeter(6, 1.second, 100)(timer)
      val greedy = AsyncMeter.extraWideAwait(18, meter)
      val first = meter.await(6)
      assert(!greedy.isDefined)
      assert(!first.isDefined)

      val e = new Exception("boom!")
      greedy.raise(e)
      assert(greedy.isDefined)
      val actual = intercept[CancellationException]
        Await.result(greedy, 5.seconds)
      assert(actual.getCause == e)

      ctl.advance(1.second)
      timer.tick()
      assert(first.isDone)
