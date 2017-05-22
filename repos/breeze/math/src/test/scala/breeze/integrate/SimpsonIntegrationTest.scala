package breeze.integrate

import org.scalatest.FunSuite

import breeze.integrate
import breeze.linalg._
import breeze.numerics._

/**
  *
  * @author chrismedrela
  **/
class SimpsonIntegrationTest extends FunSuite
  val f = (x: Double) => 2 * x
  val f2 = (x: Double) => x * x

  test("basics")
    assert(closeTo(integrate.simpson(f, 0, 1, 2), 1.0))
    assert(closeTo(integrate.simpson(f, 0, 1, 3), 1.0))
    assert(closeTo(integrate.simpson(f2, 0, 1, 2), 0.33333333333333))
    assert(closeTo(integrate.simpson(f2, 0, 1, 3), 0.33333333333333))

  test("not enough nodes")
    intercept[Exception]
      integrate.simpson(f, 0, 1, 1)
