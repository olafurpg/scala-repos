package org.jetbrains.plugins.scala
package codeInsight.generation

/**
  * Nikolay.Tropin
  * 8/23/13
  */
class GenerateEqualsTest extends ScalaGenerateTestBase
  val handler = new ScalaGenerateEqualsHandler

  def testFindAllFields()
    val text = s"""class A (i: Int, val j: Int) {
                 |  val x = 0$CARET_MARKER
                 |  var y = 0
                 |  private val z = 0
                 |}"""
    val result = """class A (i: Int, val j: Int) {
                   |  val x = 0
                   |  var y = 0
                   |  private val z = 0
                   |
                   |  def canEqual(other: Any): Boolean = other.isInstanceOf[A]
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: A =>
                   |      (that canEqual this) &&
                   |        x == that.x &&
                   |        y == that.y &&
                   |        z == that.z &&
                   |        j == that.j
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(x, z, j)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}"""
    testInvoke(text, result, checkCaret = false)

  def testInFinalClass()
    val text = s"""final class$CARET_MARKER A (i: Int, val j: Int) {
                 |  private val z = 0
                 |}"""
    val result = """final class A (i: Int, val j: Int) {
                   |  private val z = 0
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: A =>
                   |      z == that.z &&
                   |        j == that.j
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(z, j)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}"""
    testInvoke(text, result, checkCaret = false)

  def testInAbstract()
    val text = s"""abstract class A (i: Int, val j: Int) extends Set[Int] {
                 |  private val z = 0
                 |
                 |$CARET_MARKER}"""
    val result = s"""abstract class A (i: Int, val j: Int) extends Set[Int] {
                   |  private val z = 0
                   |
                   |  override def canEqual(other: Any): Boolean = other.isInstanceOf[A]
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: A =>
                   |      super.equals(that) &&
                   |        (that canEqual this) &&
                   |        z == that.z &&
                   |        j == that.j
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(super.hashCode(), z, j)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}"""
    testInvoke(text, result, checkCaret = false)

  def testInInheritor()
    val text = s"""class A {
                 |  val a = 0
                 |
                 |  def canEqual(other: Any): Boolean = other.isInstanceOf[A]
                 |
                 |  override def equals(other: Any): Boolean = other match {
                 |    case that: A =>
                 |      (that canEqual this) &&
                 |        a == that.a
                 |    case _ => false
                 |  }
                 |
                 |  override def hashCode(): Int = {
                 |    val state = Seq(a)
                 |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                 |  }
                 |}
                 |
                 |class B (i: Int, val j: Int) extends A {
                 |  val z = 0$CARET_MARKER
                 |}"""
    val result =
      """class A {
                   |  val a = 0
                   |
                   |  def canEqual(other: Any): Boolean = other.isInstanceOf[A]
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: A =>
                   |      (that canEqual this) &&
                   |        a == that.a
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(a)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}
                   |
                   |class B (i: Int, val j: Int) extends A {
                   |  val z = 0
                   |
                   |  override def canEqual(other: Any): Boolean = other.isInstanceOf[B]
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: B =>
                   |      super.equals(that) &&
                   |        (that canEqual this) &&
                   |        z == that.z &&
                   |        j == that.j
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(super.hashCode(), z, j)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}"""
    testInvoke(text, result, checkCaret = false)

  def testInheritsMethodsFromJavaLangObject() =
    val text = s"""class A {
                 |  val a = 0
                 |}
                 |
                 |class B (i: Int, val j: Int) extends A {
                 |  val z = 0$CARET_MARKER
                 |}"""
    val result = """class A {
                   |  val a = 0
                   |}
                   |
                   |class B (i: Int, val j: Int) extends A {
                   |  val z = 0
                   |
                   |  def canEqual(other: Any): Boolean = other.isInstanceOf[B]
                   |
                   |  override def equals(other: Any): Boolean = other match {
                   |    case that: B =>
                   |      (that canEqual this) &&
                   |        z == that.z &&
                   |        j == that.j
                   |    case _ => false
                   |  }
                   |
                   |  override def hashCode(): Int = {
                   |    val state = Seq(z, j)
                   |    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
                   |  }
                   |}"""
    testInvoke(text, result, checkCaret = false)
