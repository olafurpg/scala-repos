package spire
package algebra

/**
  * A module generalizes a vector space by requiring its scalar need only form
  * a ring, rather than a field.
  */
trait Module[V, @sp(Int, Long, Float, Double) R]
    extends Any with AdditiveAbGroup[V]
  implicit def scalar: Rng[R]

  def timesl(r: R, v: V): V
  def timesr(v: V, r: R): V = timesl(r, v)

object Module
  @inline final def apply[V, @sp(Int, Long, Float, Double) R](
      implicit V: Module[V, R]): Module[V, R] = V

  implicit def IdentityModule[@sp(Int, Long, Float, Double) V](
      implicit ring: Ring[V]): IdentityModule[V] =
    new IdentityModule[V]
      val scalar = ring

private[algebra] trait IdentityModule[@sp(Int, Long, Float, Double) V]
    extends Any with Module[V, V]
  def zero: V = scalar.zero
  def negate(v: V): V = scalar.negate(v)
  def plus(v: V, w: V): V = scalar.plus(v, w)
  override def minus(v: V, w: V): V = scalar.minus(v, w)

  def timesl(r: V, v: V): V = scalar.times(r, v)

final case class ZModule[V](vector: Group[V]) extends Module[V, Int]
  def scalar: Rng[Int] = spire.std.int.IntAlgebra

  def zero: V = vector.id
  def negate(v: V): V = vector.inverse(v)
  def plus(v: V, w: V): V = vector.op(v, w)
  def timesl(k: Int, v: V): V = vector.combinen(v, k)
