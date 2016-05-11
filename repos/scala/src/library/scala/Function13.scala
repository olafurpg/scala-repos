/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

/** A function of 13 parameters.
  *
  */
trait Function13[
    -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R]
    extends AnyRef { self =>

  /** Apply the body of this function to the arguments.
    *  @return   the result of function application.
    */
  def apply(v1: T1,
            v2: T2,
            v3: T3,
            v4: T4,
            v5: T5,
            v6: T6,
            v7: T7,
            v8: T8,
            v9: T9,
            v10: T10,
            v11: T11,
            v12: T12,
            v13: T13): R

  /** Creates a curried version of this function.
    *
    *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)`
    */
  @annotation.unspecialized
  def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => R = {
    (x1: T1) =>
      ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9,
      x10: T10, x11: T11, x12: T12, x13: T13) =>
        self.apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)).curried
  }

  /** Creates a tupled version of this function: instead of 13 arguments,
    *  it accepts a single [[scala.Tuple13]] argument.
    *
    *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)) == f(Tuple13(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)`
    */
  @annotation.unspecialized
  def tupled: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] => R = {
    case Tuple13(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =>
      apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  }
  override def toString() = "<function13>"
}
