package spire
package algebra
package lattice

trait Heyting[@sp(Boolean, Byte, Short, Int, Long) A]
    extends Any with BoundedLattice[A]
  def and(a: A, b: A): A
  def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  def join(a: A, b: A): A = or(a, b)

  def imp(a: A, b: A): A
  def complement(a: A): A

object Heyting
  @inline final def apply[@sp(Boolean, Byte, Short, Int, Long) A](
      implicit ev: Heyting[A]): Heyting[A] = ev
