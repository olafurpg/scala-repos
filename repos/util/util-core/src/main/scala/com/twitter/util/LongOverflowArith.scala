package com.twitter.util

@deprecated("Use Java's java.lang.ArithmeticException instead", "2015-11-16")
class LongOverflowException(msg: String) extends Exception(msg)

object LongOverflowArith

  @deprecated("Use Java 8's Math.addExact instead", "2015-11-16")
  def add(a: Long, b: Long) =
    val c = a + b
    if (((a ^ c) & (b ^ c)) < 0) throw new LongOverflowException(a + " + " + b)
    else c

  @deprecated("Use Java 8's Math.subtractExact instead", "2015-11-16")
  def sub(a: Long, b: Long) =
    val c = a - b
    if (((a ^ c) & (-b ^ c)) < 0)
      throw new LongOverflowException(a + " - " + b)
    else c

  @deprecated("Use Java 8's Math.multiplyExact instead", "2015-11-16")
  def mul(a: Long, b: Long): Long =
    if (a > b)
      // normalize so that a <= b to keep conditionals to a minimum
      mul(b, a)
    else if (a < 0L)
      if (b < 0L)
        if (a < Long.MaxValue / b)
          throw new LongOverflowException(a + " * " + b)
      else if (b > 0L)
        if (Long.MinValue / b > a)
          throw new LongOverflowException(a + " * " + b)
    else if (a > 0L)
      // and b > 0L
      if (a > Long.MaxValue / b) throw new LongOverflowException(a + " * " + b)

    a * b
