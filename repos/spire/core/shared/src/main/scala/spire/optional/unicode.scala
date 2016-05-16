package spire
package optional

import spire.algebra._
import spire.algebra.lattice._
import spire.math._

import spire.macros.Ops

package object unicode {

  type ℍ = Quaternion[Real]
  type ℂ = Complex[Real]
  type ℝ = Real
  type ℚ = Rational
  type ℤ = SafeLong
  type ℕ = Natural

  val ℝ = Real
  val ℚ = Rational
  val ℤ = SafeLong
  val ℕ = Natural

  val ⅇ = Real.e
  val π = Real.pi
  val φ = Real.phi
  val ⅈ = Complex.i[Real]
  val ⅉ = Quaternion.j[Real]

  def ⊤[A](implicit ev: Heyting[A]): A = ev.one
  def ⊥[A](implicit ev: Heyting[A]): A = ev.zero
  def ¬[A](a: A)(implicit ev: Heyting[A]): A = ev.complement(a)
  def √[A](a: A)(implicit ev: NRoot[A]): A = ev.sqrt(a)
  def ∛[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 3)
  def ∜[A](a: A)(implicit ev: NRoot[A]): A = ev.nroot(a, 4)

  def Σ[A](as: Iterable[A])(implicit ev: AdditiveMonoid[A]): A =
    as.aggregate(ev.zero)(ev.plus, ev.plus)

  def Π[A](as: Iterable[A])(implicit ev: MultiplicativeMonoid[A]): A =
    as.aggregate(ev.one)(ev.times, ev.times)

  implicit class TimesOp[A](lhs: A)(implicit ev: MultiplicativeSemigroup[A]) {
    def ∙(rhs: A): A = ev.times(lhs, rhs)
  }

  implicit class EqOps[A](lhs: A)(implicit ev: Eq[A]) {
    def ≡(rhs: A): Boolean = macro Ops.binop[A, A]
    def ≠(rhs: A): Boolean = macro Ops.binop[A, A]
  }

  implicit class PartialOrderOps[A](lhs: A)(implicit ev: PartialOrder[A]) {
    def ≤(rhs: A): Boolean = macro Ops.binop[A, A]
    def ≥(rhs: A): Boolean = macro Ops.binop[A, A]
  }

  implicit class MeetSemilatticeOps[A](lhs: A)(implicit ev: MeetSemilattice[A]) {
    def ∧(rhs: A): A = macro Ops.binop[A, A]
  }

  implicit class JoinSemilatticeOps[A](lhs: A)(implicit ev: JoinSemilattice[A]) {
    def ∨(rhs: A): A = macro Ops.binop[A, A]
  }

  implicit class HeytingOps[A](lhs: A)(implicit ev: Heyting[A]) {
    def ⊃(rhs: A): A = macro Ops.binop[A, A]
  }

  implicit class BoolOps[A](lhs: A)(implicit ev: Bool[A]) {
    def ⊻(rhs: A): A = macro Ops.binop[A, A]
    def ⊼(rhs: A): A = macro Ops.binop[A, A]
    def ⊽(rhs: A): A = macro Ops.binop[A, A]
  }

  implicit class SymbolicSetOps[A](val lhs: Set[A]) extends AnyVal {
    def ∋(a: A): Boolean = lhs(a)
    def ∌(a: A): Boolean = !lhs(a)

    def ∈:(a: A): Boolean = lhs(a)
    def ∉:(a: A): Boolean = !lhs(a)

    def ∩(rhs: Set[A]): Set[A] = lhs & rhs
    def ∪(rhs: Set[A]): Set[A] = lhs | rhs
    def \(rhs: Set[A]): Set[A] = lhs -- rhs

    def ⊂(rhs: Set[A]): Boolean = lhs.size < rhs.size && lhs.forall(rhs)
    def ⊃(rhs: Set[A]): Boolean = lhs.size > rhs.size && rhs.forall(lhs)

    def ⊆(rhs: Set[A]): Boolean = lhs.size <= rhs.size && lhs.forall(rhs)
    def ⊇(rhs: Set[A]): Boolean = lhs.size >= rhs.size && rhs.forall(lhs)
  }
}
