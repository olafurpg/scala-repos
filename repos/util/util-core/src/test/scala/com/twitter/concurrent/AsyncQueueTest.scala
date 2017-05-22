package com.twitter.concurrent

import com.twitter.util.{Await, Return, Throw}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class AsyncQueueTest extends FunSuite
  test("queue pollers")
    val q = new AsyncQueue[Int]

    val p0 = q.poll()
    val p1 = q.poll()
    val p2 = q.poll()

    assert(!p0.isDefined)
    assert(!p1.isDefined)
    assert(!p2.isDefined)

    q.offer(1)
    assert(p0.poll == Some(Return(1)))
    assert(!p1.isDefined)
    assert(!p2.isDefined)

    q.offer(2)
    assert(p1.poll == Some(Return(2)))
    assert(!p2.isDefined)

    q.offer(3)
    assert(p2.poll == Some(Return(3)))

  test("queue offers")
    val q = new AsyncQueue[Int]

    q.offer(1)
    q.offer(2)
    q.offer(3)

    assert(q.poll().poll == Some(Return(1)))
    assert(q.poll().poll == Some(Return(2)))
    assert(q.poll().poll == Some(Return(3)))

  test("into idle state and back")
    val q = new AsyncQueue[Int]

    q.offer(1)
    assert(q.poll().poll == Some(Return(1)))

    val p = q.poll()
    assert(!p.isDefined)
    q.offer(2)
    assert(p.poll == Some(Return(2)))

    q.offer(3)
    assert(q.poll().poll == Some(Return(3)))

  test("fail pending and new pollers discard=true")
    val q = new AsyncQueue[Int]

    val exc = new Exception("sad panda")
    val p0 = q.poll()
    val p1 = q.poll()

    assert(!p0.isDefined)
    assert(!p1.isDefined)

    q.fail(exc)
    assert(p0.poll == Some(Throw(exc)))
    assert(p1.poll == Some(Throw(exc)))

    assert(q.poll().poll == Some(Throw(exc)))

  test("fail pending and new pollers discard=false")
    val q = new AsyncQueue[Int]

    val exc = new Exception("sad panda")
    val p0 = q.poll()

    assert(!p0.isDefined)

    q.offer(1)
    q.offer(2)
    q.fail(exc, false)
    q.offer(3)

    assert(p0.poll == Some(Return(1)))
    assert(q.poll().poll == Some(Return(2)))
    assert(q.poll().poll == Some(Throw(exc)))
    assert(q.poll().poll == Some(Throw(exc)))

  test("fail doesn't blow up offer")
    val q = new AsyncQueue[Int]

    val exc = new Exception
    q.fail(exc)

    q.offer(1)
    assert(q.poll().poll == Some(Throw(exc)))

  test("failure is final")
    val exc = new Exception()
    val q = new AsyncQueue[Int]()

    // observable via poll
    q.fail(exc)
    assert(q.poll().poll == Some(Throw(exc)))

    // observable via drain
    assert(q.drain() == Throw(exc))
    assert(q.poll().poll == Some(Throw(exc)))

    // not overwritable
    q.fail(new RuntimeException())
    assert(q.poll().poll == Some(Throw(exc)))

    // partially observable via offer
    assert(!q.offer(1))
    assert(q.poll().poll == Some(Throw(exc)))

  test("drain")
    val q = new AsyncQueue[Int]()
    q.offer(1)
    q.offer(2)
    q.offer(3)

    assert(1 == Await.result(q.poll()))
    assert(Return(Queue(2, 3)) == q.drain())
    assert(!q.poll().isDefined)

    q.offer(4) // this is taken by the poll
    q.offer(5)
    q.offer(6)
    val ex = new RuntimeException()
    q.fail(ex, discard = false)
    assert(Return(Seq(5, 6)) == q.drain())
    // draining an empty failed queue returns the exception
    assert(Throw(ex) == q.drain())

  test("offer at max capacity")
    val q = new AsyncQueue[Int](1)
    assert(q.offer(1)) // ok
    assert(!q.offer(2)) // over capacity
    assert(1 == q.size)
    assert(Return(Queue(1)) == q.drain())
    assert(0 == q.size)

    assert(q.offer(3)) // ok
    assert(!q.offer(4)) // over capacity
    assert(1 == q.size)
    assert(Return(Queue(3)) == q.drain())
    assert(0 == q.size)
