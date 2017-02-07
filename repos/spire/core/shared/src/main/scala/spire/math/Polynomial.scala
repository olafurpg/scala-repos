package spire
package math

import scala.collection.mutable.ArrayBuilder

import java.math.{BigDecimal => JBigDecimal, RoundingMode, MathContext}

import spire.algebra._
import spire.math.poly._
import spire.std.array._
import spire.syntax.field._
import spire.syntax.eq._
import spire.syntax.std.seq._

/**
  * Polynomial
  * A univariate polynomial class and EuclideanRing extension trait
  * for arithmetic operations. Polynomials can be instantiated using
  * any type C for which a Ring[C] and Signed[C] are in scope, with
  * exponents given by Int values. Some operations require a Field[C]
  * to be in scope.
  */
object Polynomial extends PolynomialInstances {

  def dense[@sp(Double) C: Semiring: Eq: ClassTag](
      coeffs: Array[C]): PolyDense[C] = {
    var i = coeffs.length
    while (i > 0 && (coeffs(i - 1) === Semiring[C].zero)) i -= 1
    if (i == coeffs.length) {
      new PolyDense(coeffs)
    } else {
      val cs = new Array[C](i)
      System.arraycopy(coeffs, 0, cs, 0, i)
      new PolyDense(cs)
    }
  }

  def sparse[@sp(Double) C: Semiring: Eq: ClassTag](
      data: Map[Int, C]): PolySparse[C] =
    PolySparse(data)

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](
      data: Map[Int, C]): PolySparse[C] =
    sparse(data)

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](
      terms: Iterable[Term[C]]): PolySparse[C] =
    sparse(terms.map(_.toTuple)(collection.breakOut))

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](c: C,
                                                   e: Int): PolySparse[C] =
    PolySparse.safe(Array(e), Array(c))

  import scala.util.{Try, Success, Failure}

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[@sp(Double) C: Eq: Semiring: ClassTag]: Polynomial[C] =
    PolySparse.zero[C]
  def constant[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((0, c)))
  def linear[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((1, c)))
  def linear[@sp(Double) C: Eq: Semiring: ClassTag](c1: C,
                                                    c0: C): Polynomial[C] =
    Polynomial(Map((1, c1), (0, c0)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c1: C,
                                                       c0: C): Polynomial[C] =
    Polynomial(Map((1, c1), (0, c0)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((2, c)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c2: C,
                                                       c1: C,
                                                       c0: C): Polynomial[C] =
    Polynomial(Map((2, c2), (1, c1), (0, c0)))
  def cubic[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((3, c)))
  def cubic[@sp(Double) C: Eq: Semiring: ClassTag](c3: C,
                                                   c2: C,
                                                   c1: C,
                                                   c0: C): Polynomial[C] =
    Polynomial(Map((3, c3), (2, c2), (1, c1), (0, c0)))
  def one[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    constant(Rig[C].one)
  def x[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one)
  def twox[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one + Rig[C].one)

  private[this] val termRe =
    "([0-9]+\\.[0-9]+|[0-9]+/[0-9]+|[0-9]+)?(?:([a-z])(?:\\^([0-9]+))?)?".r

  private[this] val operRe = " *([+-]) *".r

  private[spire] def parse(s: String): Polynomial[Rational] = {

    // represents a term, plus a named variable v
    case class T(c: Rational, v: String, e: Int)

    // parse all the terms and operators out of the string
    @tailrec def parse(s: String, ts: List[T]): List[T] =
      if (s.isEmpty) {
        ts
      } else {
        val (op, s2) = operRe.findPrefixMatchOf(s) match {
          case Some(m) => (m.group(1), s.substring(m.end))
          case None =>
            if (ts.isEmpty) ("+", s) else throw new IllegalArgumentException(s)
        }

        val m2 = termRe
          .findPrefixMatchOf(s2)
          .getOrElse(throw new IllegalArgumentException(s2))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t = try {
          T(Rational(c), v, e.toInt)
        } catch {
          case _: Exception =>
            throw new IllegalArgumentException(s"illegal term: $c*x^$e")
        }
        parse(s2.substring(m2.end), if (t.c == 0) ts else t :: ts)
      }

    // do some pre-processing to remove whitespace/outer parens
    val t = s.trim
    val u =
      if (t.startsWith("(") && t.endsWith(")")) t.substring(1, t.length - 1)
      else t
    val v = Term.removeSuperscript(u)

    // parse out the terms
    val ts = parse(v, Nil)

    // make sure we have at most one variable
    val vs = ts.view.map(_.v).toSet.filter(_ != "")
    if (vs.size > 1)
      throw new IllegalArgumentException(
        "only univariate polynomials supported")

    // we're done!
    (Polynomial.zero[Rational] /: ts)((a, t) => a + Polynomial(t.c, t.e))
  }

  private final def split[@sp(Double) C: ClassTag](
      poly: Polynomial[C]): (Array[Int], Array[C]) = {
    val es = ArrayBuilder.make[Int]()
    val cs = ArrayBuilder.make[C]()
    poly.foreach { (e, c) =>
      es += e
      cs += c
    }
    (es.result(), cs.result())
  }

  def interpolate[C: Field: Eq: ClassTag](points: (C, C)*): Polynomial[C] = {
    def loop(p: Polynomial[C], xs: List[C], pts: List[(C, C)]): Polynomial[C] =
      pts match {
        case Nil =>
          p
        case (x, y) :: tail =>
          val c = Polynomial.constant((y - p(x)) / xs.map(x - _).qproduct)
          val prod = xs.foldLeft(Polynomial.one[C]) { (prod, xn) =>
            prod * (Polynomial.x[C] - constant(xn))
          }
          loop(p + c * prod, x :: xs, tail)
      }
    loop(Polynomial.zero[C], Nil, points.toList)
  }
}

trait Polynomial[@sp(Double) C] { lhs =>
  implicit def ct: ClassTag[C]

  /** Returns a polynmial that has a dense representation. */
  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C]

  /** Returns a polynomial that has a sparse representation. */
  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C]

  /**
    * Traverses each term in this polynomial, in order of degree, lowest to
    * highest (eg. constant term would be first) and calls `f` with the degree
    * of term and its coefficient. This may skip zero terms, or it may not.
    */
  def foreach[U](f: (Int, C) => U): Unit

  /**
    * Traverses each non-zero term in this polynomial, in order of degree, lowest
    * to highest (eg. constant term would be first) and calls `f` with the degree
    * of term and its coefficient.
    */
  def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C],
                                          eq: Eq[C]): Unit =
    foreach { (e, c) =>
      if (c =!= ring.zero) f(e, c)
    }

  /**
    * Returns the coefficients in little-endian order. So, the i-th element is
    * coeffsArray(i) * (x ** i).
    */
  def coeffsArray(implicit ring: Semiring[C]): Array[C]

  /**
    * Returns a list of non-zero terms.
    */
  def terms(implicit ring: Semiring[C], eq: Eq[C]): List[Term[C]] = {
    val lb = new scala.collection.mutable.ListBuffer[Term[C]]
    foreachNonZero { (e, c) =>
      lb += Term(c, e)
    }
    lb.result()
  }

  /**
    * Return an iterator of non-zero terms.
    *
    * This method is used to implement equals and hashCode.
    *
    * NOTE: This method uses a (_ == 0) test to prune zero values. This
    * makes sense in a context where Semiring[C] and Eq[C] are
    * unavailable, but not other places.
    */
  def termsIterator: Iterator[Term[C]]

  /** Returns a map from exponent to coefficient of this polynomial. */
  def data(implicit ring: Semiring[C], eq: Eq[C]): Map[Int, C] = {
    val bldr = new scala.collection.mutable.MapBuilder[Int, C, Map[Int, C]](
      Map.empty[Int, C])
    foreachNonZero { (e, c) =>
      bldr += ((e, c))
    }
    bldr.result()
  }

  /**
    * Returns the real roots of this polynomial.
    *
    * Depending on `C`, the `finder` argument may need to be passed "explicitly"
    * via an implicit conversion. This is because some types (eg `BigDecimal`,
    * `Rational`, etc) require an error bound, and so provide implicit
    * conversions to `RootFinder`s from the error type.  For instance,
    * `BigDecimal` requires either a scale or MathContext. So, we'd call this
    * method with `poly.roots(MathContext.DECIMAL128)`, which would return a
    * `Roots[BigDecimal` whose roots are approximated to the precision specified
    * in `DECIMAL128` and rounded appropriately.
    *
    * On the other hand, a type like `Double` doesn't require an error bound and
    * so can be called without specifying the `RootFinder`.
    *
    * @param finder a root finder to extract roots with
    * @return the real roots of this polynomial
    */
  def roots(implicit finder: RootFinder[C]): Roots[C] =
    finder.findRoots(this)

  /** Returns the coefficient of the n-th degree term. */
  def nth(n: Int)(implicit ring: Semiring[C]): C

  /** Returns the term of the highest degree in this polynomial. */
  def maxTerm(implicit ring: Semiring[C]): Term[C] =
    Term(maxOrderTermCoeff, degree)

  /**
    * Returns the non-zero term of the minimum degree in this polynomial, unless
    * it is zero. If this polynomial is zero, then this returns a zero term.
    */
  def minTerm(implicit ring: Semiring[C], eq: Eq[C]): Term[C] = {
    foreachNonZero { (n, c) =>
      return Term(c, n)
    }
    Term(ring.zero, 0)
  }

  /** Returns `true` iff this polynomial is constant. */
  def isConstant: Boolean =
    degree == 0

  /** Returns the degree of this polynomial. */
  def degree: Int

  /** Returns the coefficient of max term of this polynomial. */
  def maxOrderTermCoeff(implicit ring: Semiring[C]): C

  /** Returns a polynomial with the max term removed. */
  def reductum(implicit e: Eq[C],
               ring: Semiring[C],
               ct: ClassTag[C]): Polynomial[C]

  /** Returns `true` if this polynomial is `ring.zero`. */
  def isZero: Boolean

  /** Evaluate the polynomial at `x`. */
  def apply(x: C)(implicit r: Semiring[C]): C

  def evalWith[A: Semiring: Eq: ClassTag](x: A)(
      f: C => A)(implicit ring: Semiring[C], eq: Eq[C]): A =
    this.map(f).apply(x)

  /** Compose this polynomial with another. */
  def compose(y: Polynomial[C])(implicit ring: Rig[C],
                                eq: Eq[C]): Polynomial[C] = {
    var polynomial: Polynomial[C] = Polynomial.zero[C]
    foreachNonZero { (e, c) =>
      val z: Polynomial[C] = y.pow(e) :* c
      polynomial = polynomial + z
    }
    polynomial
  }

  /**
    * Returns this polynomial as a monic polynomial, where the leading
    * coefficient (ie. `maxOrderTermCoeff`) is 1.
    */
  def monic(implicit f: Field[C], eq: Eq[C]): Polynomial[C] =
    this :/ maxOrderTermCoeff

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C]
  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C]

  /**
    * Returns the number of sign variations in the coefficients of this
    * polynomial. Given 2 consecutive terms (ignoring 0 terms), a sign variation
    * is indicated when the terms have differing signs.
    */
  def signVariations(implicit ring: Semiring[C],
                     eq: Eq[C],
                     signed: Signed[C]): Int = {
    var prevSign: Sign = Sign.Zero
    var variations = 0
    foreachNonZero { (_, c) =>
      val sign = signed.sign(c)
      if (Sign.Zero != prevSign && sign != prevSign) {
        variations += 1
      }
      prevSign = sign
    }
    variations
  }

  /**
    * Removes all zero roots from this polynomial.
    */
  def removeZeroRoots(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val Term(_, k) = minTerm
    mapTerms { case Term(c, n) => Term(c, n - k) }
  }

  def map[D: Semiring: Eq: ClassTag](f: C => D)(implicit ring: Semiring[C],
                                                eq: Eq[C]): Polynomial[D] =
    mapTerms { case Term(c, n) => Term(f(c), n) }

  def mapTerms[D: Semiring: Eq: ClassTag](f: Term[C] => Term[D])(
      implicit ring: Semiring[C],
      eq: Eq[C]): Polynomial[D] =
    Polynomial(terms.map(f))

  /**
    * Returns this polynomial shifted by `h`. Equivalent to calling
    * `poly.compose(x + h)`.
    */
  def shift(h: C)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] =
    compose(Polynomial.x[C] + Polynomial.constant(h))

  /**
    * Translates this polynomial by `h`. Equivalent to calling
    * `poly.compose(x - h)`.
    */
  def translate(h: C)(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] =
    compose(Polynomial.x[C] - Polynomial.constant(h))

  /**
    * Replace `x`, the variable, in this polynomial with `-x`. This will
    * flip/mirror the polynomial about the y-axis.
    */
  def flip(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] =
    mapTerms {
      case term @ Term(coeff, exp) =>
        if (exp % 2 == 0) term
        else Term(-coeff, exp)
    }

  /**
    * Returns the reciprocal of this polynomial. Essentially, if this polynomial
    * is `p` with degree `n`, then returns a polynomial `q(x) = x^n*p(1/x)`.
    *
    * @see http://en.wikipedia.org/wiki/Reciprocal_polynomial
    */
  def reciprocal(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    mapTerms {
      case term @ Term(coeff, exp) =>
        Term(coeff, degree - exp)
    }

  // EuclideanRing ops.

  def unary_-()(implicit ring: Rng[C]): Polynomial[C]
  def +(rhs: Polynomial[C])(implicit ring: Semiring[C],
                            eq: Eq[C]): Polynomial[C]
  def -(rhs: Polynomial[C])(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] =
    lhs + (-rhs)
  def *(rhs: Polynomial[C])(implicit ring: Semiring[C],
                            eq: Eq[C]): Polynomial[C]
  def /~(rhs: Polynomial[C])(implicit field: Field[C],
                             eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._1
  def /%(rhs: Polynomial[C])(implicit field: Field[C],
                             eq: Eq[C]): (Polynomial[C], Polynomial[C])
  def %(rhs: Polynomial[C])(implicit field: Field[C],
                            eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._2

  def **(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = pow(k)

  def pow(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    def loop(b: Polynomial[C], k: Int, extra: Polynomial[C]): Polynomial[C] =
      if (k == 1) b * extra
      else loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) {
      throw new IllegalArgumentException("negative exponent")
    } else if (k == 0) {
      Polynomial.one[C]
    } else if (k == 1) {
      this
    } else {
      loop(this, k - 1, this)
    }
  }

  // VectorSpace ops.

  def *:(k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def :*(k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = k *: lhs
  def :/(k: C)(implicit field: Field[C], eq: Eq[C]): Polynomial[C] =
    this :* k.reciprocal

  override def hashCode: Int = {
    val it = lhs.termsIterator
    @tailrec def loop(n: Int): Int =
      if (it.hasNext) {
        val term = it.next
        loop(n ^ (0xfeed1257 * term.exp ^ term.coeff.##))
      } else n
    loop(0)
  }

  override def equals(that: Any): Boolean = that match {
    case rhs: Polynomial[_] if lhs.degree == rhs.degree =>
      val it1 = lhs.termsIterator
      val it2 = rhs.termsIterator
      @tailrec def loop(): Boolean = {
        val has1 = it1.hasNext
        val has2 = it2.hasNext
        if (has1 && has2) {
          if (it1.next == it2.next) loop() else false
        } else has1 == has2
      }
      loop()

    case rhs: Polynomial[_] =>
      false

    case n if lhs.isZero =>
      n == 0

    case n if lhs.degree == 0 =>
      val (_, lcs) = Polynomial.split(lhs)
      lcs(0) == n

    case _ =>
      false
  }

  override def toString: String =
    if (isZero) {
      "(0)"
    } else {
      val bldr = ArrayBuilder.make[Term[C]]()
      foreach { (e, c) =>
        bldr += Term(c, e)
      }

      val ts = bldr.result()
      QuickSort.sort(ts)(Order[Term[C]].reverse, implicitly[ClassTag[Term[C]]])
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}

trait PolynomialSemiring[@sp(Double) C] extends Semiring[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialRig[@sp(Double) C]
    extends PolynomialSemiring[C]
    with Rig[Polynomial[C]] {
  implicit override val scalar: Rig[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialRng[@sp(Double) C]
    extends PolynomialSemiring[C]
    with RingAlgebra[Polynomial[C], C] {
  implicit override val scalar: Rng[C]

  def timesl(r: C, v: Polynomial[C]): Polynomial[C] = r *: v
  def negate(x: Polynomial[C]): Polynomial[C] = -x
}

trait PolynomialRing[@sp(Double) C]
    extends PolynomialRng[C]
    with Ring[Polynomial[C]] {
  implicit override val scalar: Ring[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialEuclideanRing[@sp(Double) C]
    extends PolynomialRing[C]
    with EuclideanRing[Polynomial[C]]
    with VectorSpace[Polynomial[C], C] {
  implicit override val scalar: Field[C]

  override def divr(x: Polynomial[C], k: C): Polynomial[C] = x :/ k
  def quot(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x /~ y
  def mod(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x % y
  override def quotmod(x: Polynomial[C],
                       y: Polynomial[C]): (Polynomial[C], Polynomial[C]) =
    x /% y

  final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = {
    val k = spire.math.gcd(x.coeffsArray ++ y.coeffsArray)
    k *: euclid(x :/ k, y :/ k)(Polynomial.eq).monic
  }
}

trait PolynomialEq[@sp(Double) C] extends Eq[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: Polynomial[C], y: Polynomial[C]): Boolean =
    x.coeffsArray === y.coeffsArray // TODO: This is bad for sparse arrays. Do better.
}

trait PolynomialInstances0 {
  implicit def semiring[@sp(Double) C: ClassTag: Semiring: Eq]
    : PolynomialSemiring[C] =
    new PolynomialSemiring[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def eq[@sp(Double) C: ClassTag: Semiring: Eq]: PolynomialEq[C] =
    new PolynomialEq[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances1 extends PolynomialInstances0 {
  implicit def rig[@sp(Double) C: ClassTag: Rig: Eq]: PolynomialRig[C] =
    new PolynomialRig[C] {
      val scalar = Rig[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def rng[@sp(Double) C: ClassTag: Rng: Eq]: PolynomialRng[C] =
    new PolynomialRng[C] {
      val scalar = Rng[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances2 extends PolynomialInstances1 {
  implicit def ring[@sp(Double) C: ClassTag: Ring: Eq]: PolynomialRing[C] =
    new PolynomialRing[C] {
      val scalar = Ring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances3 extends PolynomialInstances2 {
  implicit def euclideanRing[@sp(Double) C: ClassTag: Field: Eq]
    : PolynomialEuclideanRing[C] =
    new PolynomialEuclideanRing[C] {
      val scalar = Field[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances extends PolynomialInstances3
