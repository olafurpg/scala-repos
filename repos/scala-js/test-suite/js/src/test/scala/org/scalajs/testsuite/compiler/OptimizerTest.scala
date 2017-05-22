/*                     __                                               *\
 **     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
 **    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
 **  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
 ** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
 **                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class OptimizerTest
  import OptimizerTest._

  // Inlineable classes

  @Test
  def must_update_fields_of_this_in_the_computation_of_other_fields_issue_1153(
      ): Unit =
    val foo = new InlineClassDependentFields(5)
    assertEquals(5, foo.x)
    assertTrue(foo.b)
    assertEquals(11, foo.y)

  @Test def must_not_break_code_that_assigns_this_to_a_field(): Unit =
    val foo = new InlineClassThisAlias(5)
    assertEquals(5, foo.z)

  // Optimizer regression tests

  @Test def `must_not_break_*_(-1)_for_Int_issue_1453`(): Unit =
    @noinline
    def start0: Int = (() => 10)()

    val start = start0
    val step = -1
    val numRangeElements = start - 1
    val lastElement = start + (numRangeElements - 1) * step
    assertEquals(2, lastElement)

  @Test def `must_not_break_*_(-1)_for_Float_and_Double_issue_1478`(): Unit =
    @noinline
    def a: Float = (() => 5.0f)()
    assertEquals(-5.0f, a * -1.0f)

    @noinline
    def b: Double = (() => 7.0)()
    assertEquals(-7.0, b * -1.0)

  @Test def must_not_break_foreach_on_downward_Range_issue_1453(): Unit =
    @noinline
    def start0: Int = (() => 10)()

    val elements = js.Array[Int]()
    for (i <- start0 to 2 by -1)
      if (i < 0) sys.error("Going into infinite loop")
      elements.push(i)
    assertArrayEquals(Array(10, 9, 8, 7, 6, 5, 4, 3, 2), elements.toArray)

  @Test def must_not_break_classOf_T_eqeq_classOf_U_issue_1658(): Unit =
    assertEquals(classOf[String], classOf[String])
    assertEquals(classOf[Int], classOf[Int])
    assertEquals(classOf[Array[Int]], classOf[Array[Int]])
    assertEquals(classOf[Array[String]], classOf[Array[String]])

    assertFalse(classOf[String] == classOf[Int])
    assertFalse(classOf[Seq[_]] == classOf[List[_]])
    assertFalse(classOf[Array[Int]] == classOf[Array[Integer]])
    assertFalse(classOf[Array[Object]] == classOf[Array[Integer]])
    assertFalse(classOf[String] == classOf[Array[String]])
    assertFalse(classOf[Array[Array[Object]]] == classOf[Array[Object]])

  // +[string] constant folding

  @Test def must_not_break_when_folding_two_constant_strings(): Unit =
    @inline def str: String = "I am "
    assertEquals("I am constant", str + "constant")

  @Test
  def must_not_break_when_folding_the_empty_string_when_associated_with_a_string(
      ): Unit =
    @noinline def str: String = "hello"
    assertEquals("hello", str + "")
    assertEquals("hello", "" + str)

  @Test def `must_not_break_when_folding_1.4f_and_a_stringLit`(): Unit =
    assertEquals("1.399999976158142hello", 1.4f + "hello")
    assertEquals("hello1.399999976158142", "hello" + 1.4f)

  @Test def must_not_break_when_folding_cascading_+[string](): Unit =
    @noinline def str: String = "awesome! 10/10"
    assertEquals("Scala.js is awesome! 10/10", "Scala.js" + (" is " + str))
    assertEquals("awesome! 10/10 is Scala.js", (str + " is ") + "Scala.js")

  @Test def must_not_break_when_folding_a_chain_of_+[string](): Unit =
    @inline def b: String = "b"
    @inline def d: String = "d"
    @inline def f: String = "f"
    assertEquals("abcdefg", "a" + b + "c" + d + "e" + f + "g")

  @Test
  def must_not_break_when_folding_integer_in_double_and_stringLit(): Unit =
    assertEquals("1hello", 1.0 + "hello")
    assertEquals("hello1", "hello" + 1.0)

  @Test def must_not_break_when_folding_zero_and_stringLit(): Unit =
    assertEquals("0hello", 0.0 + "hello")
    assertEquals("hello0", "hello" + 0.0)
    assertEquals("0hello", -0.0 + "hello")
    assertEquals("hello0", "hello" + (-0.0))

  @Test def must_not_break_when_folding_Infinities_and_stringLit(): Unit =
    assertEquals("Infinityhello", Double.PositiveInfinity + "hello")
    assertEquals("helloInfinity", "hello" + Double.PositiveInfinity)
    assertEquals("-Infinityhello", Double.NegativeInfinity + "hello")
    assertEquals("hello-Infinity", "hello" + Double.NegativeInfinity)

  @Test def must_not_break_when_folding_NaN_and_stringLit(): Unit =
    assertEquals("NaNhello", Double.NaN + "hello")
    assertEquals("helloNaN", "hello" + Double.NaN)

  @Test
  def must_not_break_when_folding_double_with_decimal_and_stringLit(): Unit =
    assumeFalse(isInFullOpt)
    assertEquals(
        "1.2323919403474454e+21hello", 1.2323919403474454E21 + "hello")
    assertEquals(
        "hello1.2323919403474454e+21", "hello" + 1.2323919403474454E21)

  @Test
  def must_not_break_when_folding_double_that_JVM_would_print_in_scientific_notation_and_stringLit(
      ): Unit =
    assumeFalse(isInFullOpt)
    assertEquals("123456789012345hello", 123456789012345d + "hello")
    assertEquals("hello123456789012345", "hello" + 123456789012345d)

  @Test def must_not_break_when_folding_doubles_to_String(): Unit =
    assumeFalse(isInFullOpt)
    @noinline def toStringNoInline(v: Double): String = v.toString
    @inline def test(v: Double): Unit =
      assertEquals(toStringNoInline(v), v.toString)

    // Special cases
    test(0.0)
    test(-0.0)
    test(Double.NaN)
    test(Double.PositiveInfinity)
    test(Double.NegativeInfinity)

    // k <= n <= 21
    test(1.0)
    test(12.0)
    test(123.0)
    test(1234.0)
    test(12345.0)
    test(123456.0)
    test(1234567.0)
    test(12345678.0)
    test(123456789.0)
    test(1234567890.0)
    test(12345678901.0)
    test(123456789012.0)
    test(1234567890123.0)
    test(12345678901234.0)
    test(123456789012345.0)
    test(1234567890123456.0)
    test(12345678901234657.0)
    test(123456789012345678.0)
    test(1234567890123456789.0)
    test(12345678901234567890.0)
    test(123456789012345678901.0)

    // 0 < n <= 21
    test(1.42)
    test(12.42)
    test(123.42)
    test(1234.42)
    test(12345.42)
    test(123456.42)
    test(1234567.42)
    test(12345678.42)
    test(123456789.42)
    test(1234567890.42)
    test(12345678901.42)
    test(123456789012.42)
    test(1234567890123.42)
    test(12345678901234.42)
    test(123456789012345.42)
    test(1234567890123456.42)
    test(12345678901234657.42)
    test(123456789012345678.42)
    test(1234567890123456789.42)
    test(12345678901234567890.42)
    test(123456789012345678901.42)

    // -6 < n <= 0
    test(0.1)
    test(0.01)
    test(0.001)
    test(0.0001)
    test(0.00001)
    test(0.000001)

    // k == 1
    test(1e22)
    test(2e25)
    test(3e50)
    test(4e100)
    test(5e200)
    test(6e300)
    test(7e307)
    test(1e-22)
    test(2e-25)
    test(3e-50)
    test(4e-100)
    test(5e-200)
    test(6e-300)
    test(7e-307)

    // else
    test(1.42e22)
    test(2.42e25)
    test(3.42e50)
    test(4.42e100)
    test(5.42e200)
    test(6.42e300)
    test(7.42e307)
    test(1.42e-22)
    test(2.42e-25)
    test(3.42e-50)
    test(4.42e-100)
    test(5.42e-200)
    test(6.42e-300)
    test(7.42e-307)

    // special cases when ulp > 1
    test(18271179521433728.0)
    test(1.15292150460684685E18)
    test(1234567890123456770.0)
    test(2234567890123456770.0)
    test(4234567890123450000.0)
    test(149170297077708820000.0)
    test(296938164846899230000.0)
    test(607681513323520000000.0)

  @Test def must_not_break_when_folding_long_and_stringLit(): Unit =
    assertEquals("1hello", 1L + "hello")
    assertEquals("hello1", "hello" + 1L)

  @Test def must_not_break_when_folding_integer_and_stringLit(): Unit =
    assertEquals("42hello", 42 + "hello")
    assertEquals("hello42", "hello" + 42)

  @Test def must_not_break_when_folding_boolean_and_stringLit(): Unit =
    assertEquals("false is not true", false + " is not true")
    assertEquals("false is not true", "false is not " + true)

  @Test def must_not_break_when_folding_unit_and_stringLit(): Unit =
    assertEquals("undefined is undefined?", () + " is undefined?")
    assertEquals("undefined is undefined", "undefined is " + ())

  @Test def must_not_break_when_folding_null_and_stringLit(): Unit =
    assertEquals("Damien is not null", "Damien is not " + null)

  @Test def must_not_break_when_folding_char_and_stringLit(): Unit =
    assertEquals("Scala.js", 'S' + "cala.js")
    assertEquals("Scala.js", "Scala.j" + 's')

object OptimizerTest

  @inline
  class InlineClassDependentFields(val x: Int)
    val b = x > 3
    val y = if (b) x + 6 else x - 2

  @inline
  class InlineClassThisAlias(val x: Int)
    val t = this
    val y = x
    val z = t.y
