package com.twitter.io

import com.twitter.util.{Future, Return, Try, Throw}

/**
  * Construct a Reader from a Buf.
  */
private[io] class BufReader(buf: Buf) extends Reader
  @volatile private[this] var state: Try[Buf] = Return(buf)

  def read(n: Int) = synchronized
    state match
      case Return(buf) =>
        if (buf.isEmpty) Future.None
        else
          val f = Future.value(Some(buf.slice(0, n)))
          state = Return(buf.slice(n, Int.MaxValue))
          f
      case Throw(exc) => Future.exception(exc)

  def discard() = synchronized
    state = Throw(new Reader.ReaderDiscarded)

object BufReader
  def apply(buf: Buf): Reader =
    if (buf.isEmpty) Reader.Null else new BufReader(buf)
