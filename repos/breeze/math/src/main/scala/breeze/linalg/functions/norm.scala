package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
  * Computes the norm of an object. Many tensor objects have a norm implementation implicit, which is what this calls.
  */
object norm extends UFunc
  @expand
  @expand.valify
  implicit def scalarNorm[@expand.args(Int, Long, Float, Double) T]: Impl[
      T, Double] = new Impl[T, Double]
    def apply(v1: T): Double = v1.abs.toDouble

  implicit def normalNormToNormUnit[T](
      implicit normImpl: Impl[T, Double]): Impl2[T, Unit, Double] =
    new Impl2[T, Unit, Double]
      def apply(v: T, x: Unit): Double = normImpl(v)

  implicit def normDoubleToNormalNorm[T](
      implicit normImpl: Impl2[T, Double, Double]): Impl[T, Double] =
    new Impl[T, Double]
      def apply(v: T): Double = normImpl(v, 2.0)

  implicit def fromCanNormInt[T](
      implicit impl: Impl2[T, Double, Double]): Impl2[T, Int, Double] =
    new Impl2[T, Int, Double]
      def apply(v: T, v2: Int): Double = impl(v, v2)

  implicit def fromCanNormFloat[T](
      implicit impl: Impl2[T, Double, Double]): Impl2[T, Float, Float] =
    new Impl2[T, Float, Float]
      def apply(v: T, v2: Float): Float = impl(v, v2).toFloat
