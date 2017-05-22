package com.twitter.util

import scala.util.Random

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LongOverflowArithTest extends WordSpec
  "LongOverflowArith" should
    val random = new Random
    val maxSqrt = 3037000499L

    def randLong() =
      if (random.nextInt > 0) random.nextLong() % maxSqrt
      else random.nextLong()

    "add" in
      def test(a: Long, b: Long)
        val bigC = BigInt(a) + BigInt(b)
        if (bigC.abs > Long.MaxValue)
          intercept[LongOverflowException]
            LongOverflowArith.add(a, b)
          else assert(LongOverflowArith.add(a, b) == bigC.toLong)

      for (i <- 0 until 1000)
        test(randLong(), randLong())

    "sub" in
      def test(a: Long, b: Long)
        val bigC = BigInt(a) - BigInt(b)
        if (bigC.abs > Long.MaxValue)
          intercept[LongOverflowException]
            LongOverflowArith.sub(a, b)
          else assert(LongOverflowArith.sub(a, b) == bigC.toLong)

      for (i <- 0 until 1000)
        test(randLong(), randLong())

    "mul" in
      assert(LongOverflowArith.mul(0L, 10L) == 0L)
      assert(LongOverflowArith.mul(1L, 11L) == 11L)
      assert(LongOverflowArith.mul(-1L, -11L) == 11L)
      assert(LongOverflowArith.mul(-1L, 22L) == -22L)
      assert(LongOverflowArith.mul(22L, -1L) == -22L)

      intercept[LongOverflowException]
        LongOverflowArith.mul(3456116450671355229L, -986247066L)

      assert(LongOverflowArith.mul(Long.MaxValue, 1L) == Long.MaxValue)

      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MaxValue - 1L, 9L)

      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue, 2L)
      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue, -2L)
      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue, 3L)
      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue, -3L)
      assert(LongOverflowArith.mul(Long.MinValue, 1L) == Long.MinValue)
      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue, -1L)
      assert(LongOverflowArith.mul(1L, Long.MinValue) == Long.MinValue)
      intercept[LongOverflowException]
        LongOverflowArith.mul(-1L, Long.MinValue)
      assert(LongOverflowArith.mul(Long.MinValue, 0L) == 0L)
      intercept[LongOverflowException]
        LongOverflowArith.mul(Long.MinValue + 1L, 2L)

      def test(a: Long, b: Long)
        val bigC = BigInt(a) * BigInt(b)
        if (bigC.abs > Long.MaxValue)
          intercept[LongOverflowException]
            LongOverflowArith.mul(a, b)
          else assert(LongOverflowArith.mul(a, b) == bigC.toLong)

      for (i <- 0 until 1000)
        val a = randLong()
        val b = randLong()
        try
          test(a, b)
        catch
          case x: Throwable =>
              println(a + " * " + b + " failed")
              throw x
