/*                     __                                               *\
 **     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
 **    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
 **  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
 ** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
 **                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import java.lang.{Long => JLong}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

/** Tests the implementation of the java standard library Long
  *  requires jsinterop/LongTest to work to make sense
  */
class LongTest {

  @Test def reverseBytes(): Unit = {
    assertEquals(0X14FF01D49C68ABF5L, JLong.reverseBytes(0XF5AB689CD401FF14L))
  }

  @Test def rotateLeft(): Unit = {
    assertEquals(0XF5AB689CD401FF14L, JLong.rotateLeft(0XF5AB689CD401FF14L, 0))
    assertEquals(0XEB56D139A803FE29L, JLong.rotateLeft(0XF5AB689CD401FF14L, 1))
    assertEquals(0XAB689CD401FF14F5L, JLong.rotateLeft(0XF5AB689CD401FF14L, 8))
    assertEquals(0X6D139A803FE29EB5L, JLong.rotateLeft(0XF5AB689CD401FF14L, 13))
    assertEquals(0XF5AB689CD401FF14L, JLong.rotateLeft(0XF5AB689CD401FF14L, 64))
    assertEquals(0XEB56D139A803FE29L, JLong.rotateLeft(0XF5AB689CD401FF14L, 65))
    assertEquals(0X689CD401FF14F5ABL, JLong.rotateLeft(0XF5AB689CD401FF14L, 80))
    assertEquals(0X7AD5B44E6A00FF8AL, JLong.rotateLeft(0XF5AB689CD401FF14L, -1))
    assertEquals(
      0XAB689CD401FF14F5L,
      JLong.rotateLeft(0XF5AB689CD401FF14L, -56))
    assertEquals(
      0X53D6ADA2735007FCL,
      JLong.rotateLeft(0XF5AB689CD401FF14L, -70))
  }

  @Test def rotateRight(): Unit = {
    assertEquals(0XF5AB689CD401FF14L, JLong.rotateRight(0XF5AB689CD401FF14L, 0))
    assertEquals(0X7AD5B44E6A00FF8AL, JLong.rotateRight(0XF5AB689CD401FF14L, 1))
    assertEquals(0X14F5AB689CD401FFL, JLong.rotateRight(0XF5AB689CD401FF14L, 8))
    assertEquals(
      0XF8A7AD5B44E6A00FL,
      JLong.rotateRight(0XF5AB689CD401FF14L, 13))
    assertEquals(
      0XF5AB689CD401FF14L,
      JLong.rotateRight(0XF5AB689CD401FF14L, 64))
    assertEquals(
      0X7AD5B44E6A00FF8AL,
      JLong.rotateRight(0XF5AB689CD401FF14L, 65))
    assertEquals(
      0XFF14F5AB689CD401L,
      JLong.rotateRight(0XF5AB689CD401FF14L, 80))
    assertEquals(
      0XEB56D139A803FE29L,
      JLong.rotateRight(0XF5AB689CD401FF14L, -1))
    assertEquals(
      0X14F5AB689CD401FFL,
      JLong.rotateRight(0XF5AB689CD401FF14L, -56))
    assertEquals(
      0X6ADA2735007FC53DL,
      JLong.rotateRight(0XF5AB689CD401FF14L, -70))
  }

  @Test def bitCount(): Unit = {
    assertEquals(0, JLong.bitCount(0L))
    assertEquals(26, JLong.bitCount(35763829229342837L))
    assertEquals(32, JLong.bitCount(-350003829229342837L))
  }

  @Test def compareTo(): Unit = {
    def compare(x: Long, y: Long): Int =
      new JLong(x).compareTo(new JLong(y))

    assertTrue(compare(0L, 5L) < 0)
    assertTrue(compare(10L, 9L) > 0)
    assertTrue(compare(-2L, -1L) < 0)
    assertEquals(0, compare(3L, 3L))
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0L, 5L) < 0)
    assertTrue(compare(10L, 9L) > 0)
    assertTrue(compare(-2L, -1L) < 0)
    assertEquals(0, compare(3L, 3L))
  }

  @Test def should_parse_strings(): Unit = {
    def test(s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s))
      assertEquals(v, JLong.valueOf(s).longValue())
      assertEquals(v, new JLong(s).longValue())
    }

    test("0", 0L)
    test("5", 5L)
    test("127", 127L)
    test("-100", -100L)
    test("30000", 30000L)
    test("-90000", -90000L)
    test("4", 4L)
    test("-4", -4L)
    test("4000000000", 4000000000L)
    test("-18014398509482040", -18014398509482040L)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JLong.parseLong(s))

    test("abc")
    test("asdf")
    test("")
  }

  @Test def should_parse_strings_in_base_16(): Unit = {
    def test(s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s, 16))
      assertEquals(v, JLong.valueOf(s, 16).longValue())
    }

    test("0", 0X0L)
    test("5", 0X5L)
    test("ff", 0XFFL)
    test("-24", -0X24L)
    test("30000", 0X30000L)
    test("-90000", -0X90000L)
    test("bfc94973", 3217639795L)
    test("bfc949733", 51482236723L)
  }

  @Test def should_parse_strings_in_bases_2_to_36(): Unit = {
    def test(radix: Int, s: String, v: Long): Unit = {
      assertEquals(v, JLong.parseLong(s, radix))
      assertEquals(v, JLong.valueOf(s, radix).longValue())
    }

    def genTestValue(i: Int): Long = {
      val result = Long.MaxValue / (1L << i)
      if (i > 63) -result
      else result
    }

    for {
      radix <- 2 to 36
      i <- 0 until 128
    } {
      val n = genTestValue(i)
      test(radix, JLong.toString(n, radix), n)
    }
  }

  @Test
  def should_reject_parsing_strings_when_base_less_than_2_or_base_larger_than_36(
      ): Unit = {
    def test(s: String, radix: Int): Unit = {
      expectThrows(classOf[NumberFormatException], JLong.parseLong(s, radix))
      expectThrows(
        classOf[NumberFormatException],
        JLong.valueOf(s, radix).longValue())
    }

    List[Int](-10, -5, 0, 1, 37, 38, 50, 100).foreach(test("5", _))
  }

  @Test def toString_without_radix(): Unit = {
    assertEquals("2147483647", Int.MaxValue.toLong.toString)
    assertEquals("-50", (-50L).toString)
    assertEquals("-1000000000", (-1000000000L).toString)
    assertEquals("2147483648", (Int.MaxValue.toLong + 1L).toString)
    assertEquals("-2147483648", Int.MinValue.toLong.toString)

    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/JLongTest.java
     */
    assertEquals("89000000005", new JLong(89000000005L).toString)
    assertEquals("-9223372036854775808", new JLong(JLong.MIN_VALUE).toString)
    assertEquals("9223372036854775807", new JLong(JLong.MAX_VALUE).toString)
    assertEquals("-80765", JLong.toString(-80765L))
    assertEquals("80765", JLong.toString(80765L))
    assertEquals("-2147483648", JLong.toString(Integer.MIN_VALUE.toLong))
    assertEquals("2147483647", JLong.toString(Integer.MAX_VALUE.toLong))
    assertEquals("-89000000005", JLong.toString(-89000000005L))
    assertEquals("89000000005", JLong.toString(89000000005L))
    assertEquals("-9223372036854775808", JLong.toString(JLong.MIN_VALUE))
    assertEquals("9223372036854775807", JLong.toString(JLong.MAX_VALUE))
  }

  @Test def toString_with_radix(): Unit = {
    /* Ported from
     * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/JLongTest.java
     */
    assertEquals("100000000", JLong.toString(100000000L, 10))
    assertEquals("77777777777", JLong.toString(8589934591L, 8))
    assertEquals("fffffffff", JLong.toString(68719476735L, 16))
    assertEquals(
      "1111111111111111111111111111111111111111111",
      JLong.toString(8796093022207L, 2))
    assertEquals(
      "-9223372036854775808",
      JLong.toString(0X8000000000000000L, 10))
    assertEquals("9223372036854775807", JLong.toString(0X7FFFFFFFFFFFFFFFL, 10))
    assertEquals("-8000000000000000", JLong.toString(0X8000000000000000L, 16))
    assertEquals("7fffffffffffffff", JLong.toString(0X7FFFFFFFFFFFFFFFL, 16))
  }

  @Test def highestOneBit(): Unit = {
    assertEquals(0L, JLong.highestOneBit(0L))
    assertEquals(Long.MinValue, JLong.highestOneBit(-1L))
    assertEquals(Long.MinValue, JLong.highestOneBit(-256L))
    assertEquals(1L, JLong.highestOneBit(1L))
    assertEquals(0X80L, JLong.highestOneBit(0X88L))
    assertEquals(0X4000000000000000L, JLong.highestOneBit(Long.MaxValue))
    assertEquals(Long.MinValue, JLong.highestOneBit(Long.MinValue))
    assertEquals(0X20000000000L, JLong.highestOneBit(0X32100012300L))
  }

  @Test def lowestOneBit(): Unit = {
    assertEquals(0L, JLong.lowestOneBit(0L))
    assertEquals(1L, JLong.lowestOneBit(-1L))
    assertEquals(256L, JLong.lowestOneBit(-256L))
    assertEquals(4L, JLong.lowestOneBit(12L))
    assertEquals(0X8L, JLong.lowestOneBit(0X88L))
    assertEquals(1L, JLong.lowestOneBit(Long.MaxValue))
    assertEquals(Long.MinValue, JLong.lowestOneBit(Long.MinValue))
    assertEquals(0X100L, JLong.lowestOneBit(0X32100012300L))
  }

  @Test def toBinaryString(): Unit = {
    assertEquals("0", JLong.toBinaryString(0L))
    assertEquals(
      "1111111111111111111111111111111111111111111111111111111111111111",
      JLong.toBinaryString(-1L))
    assertEquals(
      "11011001100101111010101100110",
      JLong.toBinaryString(456324454L))
    assertEquals(
      "1111111111111111111111111111111111100100110011010000101010011010",
      JLong.toBinaryString(-456324454L))
    assertEquals(
      "10110011101001110011110011111111111101001111101",
      JLong.toBinaryString(98765432158845L))
    assertEquals(
      "1111111111111111110100101110100101011001100101101001000111001100",
      JLong.toBinaryString(-49575304457780L))
    assertEquals(
      "1000000000000000000000000000000000000000000000000000000000000000",
      JLong.toBinaryString(Long.MinValue))
    assertEquals(
      "111111111111111111111111111111111111111111111111111111111111111",
      JLong.toBinaryString(Long.MaxValue))
  }

  @Test def toHexString(): Unit = {
    assertEquals("0", JLong.toHexString(0L))
    assertEquals("ffffffffffffffff", JLong.toHexString(-1L))
    assertEquals("1b32f566", JLong.toHexString(456324454L))
    assertEquals("ffffffffe4cd0a9a", JLong.toHexString(-456324454L))
    assertEquals("59d39e7ffa7d", JLong.toHexString(98765432158845L))
    assertEquals("ffffd2e9599691cc", JLong.toHexString(-49575304457780L))
    assertEquals("8000000000000000", JLong.toHexString(Long.MinValue))
    assertEquals("7fffffffffffffff", JLong.toHexString(Long.MaxValue))
  }

  @Test def toOctalString(): Unit = {
    assertEquals("0", JLong.toOctalString(0L))
    assertEquals("1777777777777777777777", JLong.toOctalString(-1L))
    assertEquals("3314572546", JLong.toOctalString(456324454L))
    assertEquals("1777777777774463205232", JLong.toOctalString(-456324454L))
    assertEquals("2635163637775175", JLong.toOctalString(98765432158845L))
    assertEquals(
      "1777776456453145510714",
      JLong.toOctalString(-49575304457780L))
    assertEquals("1000000000000000000000", JLong.toOctalString(Long.MinValue))
    assertEquals("777777777777777777777", JLong.toOctalString(Long.MaxValue))
  }

  @Test def numberOfLeadingZeros(): Unit = {
    assertEquals(0, JLong.numberOfLeadingZeros(0X9876543210ABCDEFL))
    assertEquals(6, JLong.numberOfLeadingZeros(0X272D130652A160FL))
    assertEquals(61, JLong.numberOfLeadingZeros(0X4L))
    assertEquals(13, JLong.numberOfLeadingZeros(0X645D32476A42AL))
    assertEquals(31, JLong.numberOfLeadingZeros(0X19B8ED092L))
    assertEquals(8, JLong.numberOfLeadingZeros(0XDC2D80FE481E77L))
    assertEquals(2, JLong.numberOfLeadingZeros(0X3AF189A5D0DFAE26L))
    assertEquals(23, JLong.numberOfLeadingZeros(0X151DC269439L))
    assertEquals(9, JLong.numberOfLeadingZeros(0X60E7BE653BE060L))
    assertEquals(52, JLong.numberOfLeadingZeros(0XE39L))
    assertEquals(61, JLong.numberOfLeadingZeros(0X6L))
    assertEquals(37, JLong.numberOfLeadingZeros(0X7EA26E0L))
    assertEquals(12, JLong.numberOfLeadingZeros(0X882FB98EC313BL))
    assertEquals(11, JLong.numberOfLeadingZeros(0X136EFD8F1BEEBAL))
    assertEquals(64, JLong.numberOfLeadingZeros(0X0L))
    assertEquals(58, JLong.numberOfLeadingZeros(0X3AL))
    assertEquals(4, JLong.numberOfLeadingZeros(0XC3C7ECF1E25F4B4L))
    assertEquals(57, JLong.numberOfLeadingZeros(0X48L))
    assertEquals(21, JLong.numberOfLeadingZeros(0X63C51C723A8L))
    assertEquals(50, JLong.numberOfLeadingZeros(0X2742L))
    assertEquals(39, JLong.numberOfLeadingZeros(0X10630C7L))
  }

  @Test def numberOfTrailingZeros(): Unit = {
    assertEquals(52, JLong.numberOfTrailingZeros(0XFF10000000000000L))
    assertEquals(53, JLong.numberOfTrailingZeros(0XFF20000000000000L))
    assertEquals(54, JLong.numberOfTrailingZeros(0XFF40000000000000L))
    assertEquals(55, JLong.numberOfTrailingZeros(0XFF80000000000000L))

    assertEquals(40, JLong.numberOfTrailingZeros(0X0000010000000000L))
    assertEquals(41, JLong.numberOfTrailingZeros(0X0000020000000000L))
    assertEquals(42, JLong.numberOfTrailingZeros(0X0000040000000000L))
    assertEquals(43, JLong.numberOfTrailingZeros(0X0000080000000000L))

    assertEquals(16, JLong.numberOfTrailingZeros(0X0000000000010000L))
    assertEquals(17, JLong.numberOfTrailingZeros(0X0000000000020000L))
    assertEquals(18, JLong.numberOfTrailingZeros(0X0000000000040000L))
    assertEquals(19, JLong.numberOfTrailingZeros(0X0000000000080000L))
  }

  @Test def signum(): Unit = {
    //check a few ints
    assertEquals(-1, JLong.signum(-11))
    assertEquals(-1, JLong.signum(-1))
    assertEquals(0, JLong.signum(0))
    assertEquals(1, JLong.signum(1))
    assertEquals(1, JLong.signum(11))

    //check a few longs
    assertEquals(-1, JLong.signum(Long.MinValue))
    assertEquals(-1, JLong.signum(-98765432158845L))
    assertEquals(-1, JLong.signum(-49575304457780L))
    assertEquals(-1, JLong.signum(-11L))
    assertEquals(-1, JLong.signum(-1L))
    assertEquals(0, JLong.signum(0L))
    assertEquals(1, JLong.signum(1L))
    assertEquals(1, JLong.signum(11L))
    assertEquals(1, JLong.signum(49575304457780L))
    assertEquals(1, JLong.signum(98765432158845L))
    assertEquals(1, JLong.signum(Long.MaxValue))
  }
}
