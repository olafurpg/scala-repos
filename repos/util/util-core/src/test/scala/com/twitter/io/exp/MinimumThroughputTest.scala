package com.twitter.io.exp

import com.twitter.conversions.time._
import com.twitter.io.{Writer, Buf, BufReader, Reader}
import com.twitter.util._
import java.io.ByteArrayOutputStream
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

@RunWith(classOf[JUnitRunner])
class MinimumThroughputTest extends FunSuite with MockitoSugar
  test("Reader - negative minBps")
    intercept[AssertionError]
      MinimumThroughput.reader(mock[Reader], -1, Timer.Nil)

  test("Reader - faster than min")
    val buf = Buf.UsAscii("soylent green is made of...") // 27 bytes
    val reader = MinimumThroughput.reader(BufReader(buf), 0d, Timer.Nil)

    // read from the beginning
    Await.result(reader.read(13)) match
      case Some(b) => assert(b == Buf.UsAscii("soylent green"))
      case _ => fail()

    // a no-op read
    Await.result(reader.read(0)) match
      case Some(b) => assert(b == Buf.Empty)
      case _ => fail()

    // read to the end
    Await.result(reader.read(20)) match
      case Some(b) => assert(b == Buf.UsAscii(" is made of..."))
      case _ => fail()

    // read past the end
    Await.result(reader.read(20)) match
      case None =>
      case _ => fail()

  test("Reader - below threshold after read")
    val buf = Buf.UsAscii("0")

    Time.withCurrentTimeFrozen  tc =>
      // it'd be great if we could just use a mock.
      // the problem was that stubs are evaluated once, eagerly, at creation time.
      val underlying = new Reader
        private var reads = 0
        def discard(): Unit = ()
        def read(n: Int): Future[Option[Buf]] =
          reads += 1
          if (reads == 2)
            // now take 10 seconds to read 1 more byte,
            // which would put us at 0.2 bps, and thus below the threshold.
            tc.advance(10.seconds)
          Future.value(Some(buf))

      val reader = MinimumThroughput.reader(underlying,
                                            1d, // min bytes per second
                                            Timer.Nil)

      // do a read of 1 byte in 0 time — which is ok.
      Await.result(reader.read(1)) match
        case Some(b) => assert(b == buf.slice(0, 1))
        case _ => fail()

      val ex = intercept[BelowThroughputException]
        // note in the mock above, the 2nd read takes 10 seconds
        Await.result(reader.read(1))
      assert(ex.elapsed == 10.seconds)
      assert(ex.expectedBps == 1d)
      assert(ex.currentBps == 0.2d)

  test("Reader - times out while reading")
    val underlying = mock[Reader]
    when(underlying.read(1)).thenReturn(Future.never)

    val timer = new MockTimer()
    val reader = MinimumThroughput.reader(underlying,
                                          1d, // min bytes per second
                                          timer)

    Time.withCurrentTimeFrozen  tc =>
      val f = reader.read(1)
      tc.advance(10.seconds)
      timer.tick()

      val ex = intercept[BelowThroughputException]
        Await.result(f)
      assert(ex.elapsed == 10.seconds)
      assert(ex.expectedBps == 1d)
      assert(ex.currentBps == 0d)

  test("Reader - failures from underlying reader are untouched")
    val ex = new RuntimeException("└[∵┌]└[ ∵ ]┘[┐∵]┘")
    val underlying = mock[Reader]
    when(underlying.read(1)).thenReturn(Future.exception(ex))

    val reader = MinimumThroughput.reader(underlying,
                                          1d, // min bytes per second
                                          Timer.Nil)

    val thrown = intercept[RuntimeException]
      Await.result(reader.read(1))
    assert(thrown == ex)

  test("Reader - pass through EOFs from underlying")
    val reader = MinimumThroughput.reader(Reader.Null,
                                          1d, // min bytes per second
                                          Timer.Nil)

    Await.result(reader.read(1)) match
      case None =>
      case _ => fail()

  test("Reader - discard is passed through to underlying")
    val underlying = mock[Reader]
    val reader = MinimumThroughput.reader(underlying, 1, Timer.Nil)

    reader.discard()
    verify(underlying).discard()

  test("Writer - faster than min")
    val buf = Buf.UsAscii("0")

    val writer = MinimumThroughput.writer(
        Writer.fromOutputStream(new ByteArrayOutputStream()),
        0d,
        Timer.Nil)

    val w1 = writer.write(buf)
    Await.ready(w1)
    assert(w1.isDone)

    val w2 = writer.write(buf)
    Await.ready(w2)
    assert(w2.isDone)

  test("Writer - below threshold after write")
    val buf = Buf.UsAscii("0")

    Time.withCurrentTimeFrozen  tc =>
      // it'd be great if we could just use a mock.
      // the problem was that stubs are evaluated once, eagerly, at creation time.
      val underlying = new Writer
        private var writes = 0
        def fail(cause: Throwable): Unit = ()
        def write(buf: Buf): Future[Unit] =
          writes += 1
          if (writes == 2)
            // now take 10 seconds to write 1 more byte,
            // which would put us at 0.2 bps, and thus below the threshold.
            tc.advance(10.seconds)
          Future.Done

      val writer = MinimumThroughput.writer(underlying,
                                            1d, // min bytes per second
                                            Timer.Nil)

      // do a write of 1 byte in 0 time — which is ok.
      val w1 = writer.write(buf)
      Await.ready(w1)
      assert(w1.isDone)

      val ex = intercept[BelowThroughputException]
        // note in the mock above, the 2nd write takes 10 seconds
        Await.result(writer.write(buf))
      assert(ex.elapsed == 10.seconds)
      assert(ex.expectedBps == 1d)
      assert(ex.currentBps == 0.2d)

  test("Writer - times out while writing")
    val buf = Buf.UsAscii("0")
    val underlying = mock[Writer]
    when(underlying.write(buf)).thenReturn(Future.never)

    val timer = new MockTimer()
    val writer = MinimumThroughput.writer(underlying, 1d, timer)

    Time.withCurrentTimeFrozen  tc =>
      val f = writer.write(buf)
      tc.advance(10.seconds)
      timer.tick()

      val ex = intercept[BelowThroughputException]
        Await.result(f)
      assert(ex.elapsed == 10.seconds)
      assert(ex.expectedBps == 1d)
      assert(ex.currentBps == 0d)

  test("Writer - failures from underlying writer are untouched")
    val buf = Buf.UsAscii("0")
    val ex = new RuntimeException("ᕕ( ᐛ )ᕗ")
    val underlying = mock[Writer]
    when(underlying.write(buf)).thenReturn(Future.exception(ex))

    val writer = MinimumThroughput.writer(underlying, 1d, Timer.Nil)

    val thrown = intercept[RuntimeException]
      Await.result(writer.write(buf))
    assert(thrown == ex)

  test("Writer - fail is passed through to underlying")
    val underlying = mock[Writer]

    val writer = MinimumThroughput.writer(underlying, 1d, Timer.Nil)

    val ex = new RuntimeException()
    writer.fail(ex)

    verify(underlying).fail(ex)
