package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.operators.OpDiv

/**
  * Normalizes the argument such that its norm is 1.0 (with respect to the argument n).
  * Returns value if value's norm is 0.
  */
object normalize extends UFunc
  implicit def normalizeDoubleImpl[T, U >: T](
      implicit div: OpDiv.Impl2[T, Double, U],
      canNorm: norm.Impl2[T, Double, Double]): Impl2[T, Double, U] =
    new Impl2[T, Double, U]
      def apply(t: T, n: Double): U =
        val norm = canNorm(t, n)
        if (norm == 0) t
        else div(t, norm)

  implicit def normalizeFloatImpl[T, U >: T](
      implicit div: OpDiv.Impl2[T, Float, U],
      canNorm: norm.Impl2[T, Float, Float]): Impl2[T, Float, U] =
    new Impl2[T, Float, U]
      def apply(t: T, n: Float): U =
        val norm = canNorm(t, n)
        if (norm == 0) t
        else div(t, norm)

  implicit def normalizeInPlaceDoubleImpl[T, U >: T](
      implicit div: OpDiv.InPlaceImpl2[T, Double],
      canNorm: norm.Impl2[T, Double, Double]): InPlaceImpl2[T, Double] =
    new InPlaceImpl2[T, Double]
      def apply(t: T, n: Double): Unit =
        val norm = canNorm(t, n)
        if (norm != 0) div(t, norm)

  implicit def normalizeInPlaceFloatImpl[T, U >: T](
      implicit div: OpDiv.InPlaceImpl2[T, Float],
      canNorm: norm.Impl2[T, Float, Float]): InPlaceImpl2[T, Float] =
    new InPlaceImpl2[T, Float]
      def apply(t: T, n: Float): Unit =
        val norm = canNorm(t, n)
        if (norm != 0) div(t, norm)

  implicit def normalizeImpl[T, U >: T](
      implicit impl: Impl2[T, Double, U]): Impl[T, U] =
    new Impl[T, U]
      def apply(v: T): U = impl(v, 2.0)

  implicit def normalizeIntImpl[T, U >: T](
      implicit impl: Impl2[T, Double, U]): Impl2[T, Int, U] =
    new Impl2[T, Int, U]
      def apply(v: T, n: Int): U = impl(v, n)
