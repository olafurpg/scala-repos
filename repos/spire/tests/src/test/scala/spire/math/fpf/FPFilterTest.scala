package spire
package math

import java.math.MathContext.UNLIMITED

import spire.algebra._
import spire.implicits._

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

case class Degenerate[A](value: A)

class FpFilterTest extends FunSuite with Checkers {
  final class Evaluated extends java.lang.Exception
  private def evaluated = throw new Evaluated

  // This will always error out for any operation. It can be used to ensure
  // operations are always performed with Doubles only and never fall back to
  // the exact case, since it'll fail with an Evaluated excetion.
  sealed trait Bad
  implicit object BadField
      extends Field[Bad]
      with IsReal[Bad]
      with NRoot[Bad] {
    def zero: Bad = evaluated
    def one: Bad = evaluated
    def negate(a: Bad): Bad = evaluated
    def plus(a: Bad, b: Bad): Bad = evaluated
    def quot(a: Bad, b: Bad) = evaluated
    def mod(a: Bad, b: Bad) = evaluated
    def gcd(a: Bad, b: Bad): Bad = evaluated
    override def fromDouble(n: Double): Bad = evaluated
    def times(x: Bad, b: Bad): Bad = evaluated
    def div(a: Bad, b: Bad): Bad = evaluated
    def nroot(a: Bad, k: Int): Bad = evaluated
    def fpow(a: Bad, b: Bad) = evaluated
    def compare(x: Bad, y: Bad) = evaluated
    def signum(a: Bad): Int = evaluated
    def abs(a: Bad): Bad = evaluated
    def toDouble(x: Bad): Double = evaluated
    def toReal(x: Bad): Real = evaluated
    def ceil(a: Bad): Bad = evaluated
    def floor(a: Bad): Bad = evaluated
    def round(a: Bad): Bad = evaluated
    def isWhole(a: Bad): Boolean = evaluated
  }

  test("FpFilter doesn't evaluated for easy problems") {
    val x = FpFilter.exact[Bad](1D)
    val y = FpFilter.exact[Bad](1.2D)
    assert((x + y).signum == 1)
    assert((x - y).signum == -1)
    assert((x * y).signum == 1)
    assert((x / y).signum == 1)
    assert(y.sqrt.signum == 1)
  }

  test("Find tricky zero") {
    val x = FpFilter.exact[Algebraic](18)
    val y = FpFilter.exact[Algebraic](8)
    val z = FpFilter.exact[Algebraic](2)
    assert((x.sqrt - y.sqrt - z.sqrt).signum == 0)
  }

  test("Comparisons") {
    val x = FpFilter.exact[Algebraic](-2)
    val y = FpFilter.exact[Algebraic](8)
    assert(x < y)
    assert(y > x)
    assert(x <= y)
    assert(x <= x)
    assert(y >= x)
    assert(y >= y)
    assert(x === x)
  }

  test("Mix-match macro and non-macro") {
    val x = FpFilter.exact[Algebraic](18)
    val y = FpFilter.exact[Algebraic](8)
    val z = FpFilter.exact[Algebraic](2)
    val u = x.sqrt - y.sqrt
    val v = u - z.sqrt
    assert(v.signum == 0)
  }

  case class Point(x: Double, y: Double)
  case class Simplex(p: Point, q: Point, r: Point)

  // I'm not trying to test things that won't ever work.
  def genSimpleDouble: Gen[Double] =
    for {
      n <- arbitrary[Long]
    } yield {
      (n >>> 11) * 1.1102230246251565e-16
    }

  def genPoint: Gen[Point] =
    for {
      x <- genSimpleDouble
      y <- genSimpleDouble
    } yield Point(x, y)

  def genEpsilon: Gen[Double] =
    genSimpleDouble map (_ * FpFilter.Eps)

  def genSimplex: Gen[Simplex] =
    for {
      p <- genPoint
      q <- genPoint
      r <- genPoint
    } yield Simplex(p, q, r)

  def genDegenerateSimplex: Gen[Simplex] =
    for {
      p <- genPoint
      q <- genPoint
      ex <- genEpsilon
      ey <- genEpsilon
    } yield {
      val dx = q.x - p.x
      val dy = q.y - p.y
      val r = Point(q.x + dx + ex, q.y + dy + ey)
      Simplex(p, q, r)
    }

  def signExact(s: Simplex): Int = {
    import s._
    val px = BigDecimal(p.x, UNLIMITED)
    val py = BigDecimal(p.y, UNLIMITED)
    val qx = BigDecimal(q.x, UNLIMITED)
    val qy = BigDecimal(q.y, UNLIMITED)
    val rx = BigDecimal(r.x, UNLIMITED)
    val ry = BigDecimal(r.y, UNLIMITED)
    ((qx - px) * (ry - py) - (rx - px) * (qy - py)).signum
  }

  def signFpFilter(s: Simplex): Int = {
    import s._
    val px = FpFilter.exact[BigDecimal](p.x)
    val py = FpFilter.exact[BigDecimal](p.y)
    val qx = FpFilter.exact[BigDecimal](q.x)
    val qy = FpFilter.exact[BigDecimal](q.y)
    val rx = FpFilter.exact[BigDecimal](r.x)
    val ry = FpFilter.exact[BigDecimal](r.y)
    ((qx - px) * (ry - py) - (rx - px) * (qy - py)).signum
  }

  implicit def arbSimplex: Arbitrary[Simplex] =
    Arbitrary(genSimplex)

  implicit def arbDegenerateSimplex: Arbitrary[Degenerate[Simplex]] =
    Arbitrary(genDegenerateSimplex map (new Degenerate(_)))

  test("Orientation test for simple case")(check(forAll { (s: Simplex) =>
    Sign(signExact(s)) == Sign(signFpFilter(s))
  }))

  test("Orientation test for degenerate case")(check(forAll {
    (s: Degenerate[Simplex]) =>
      Sign(signExact(s.value)) == Sign(signFpFilter(s.value))
  }))
}
