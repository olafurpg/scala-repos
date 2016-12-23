package breeze.optimize

import breeze.util._
import breeze.linalg._
import breeze.numerics._
import breeze.math._

/**
  * Implements the Orthant-wise Limited Memory QuasiNewton method,
  * which is a variant of LBFGS that handles L1 regularization.
  *
  * Paper is Andrew and Gao (2007) Scalable Training of L1-Regularized Log-Linear Models
  *
  * @author dlwh
  */
class OWLQN[K, T](maxIter: Int, m: Int, l1reg: K => Double, tolerance: Double)(
    implicit space: MutableEnumeratedCoordinateField[T, K, Double])
    extends LBFGS[T](maxIter, m, tolerance = tolerance)
    with SerializableLogging {

  def this(maxIter: Int, m: Int, l1reg: K => Double)(
      implicit space: MutableEnumeratedCoordinateField[T, K, Double]) =
    this(maxIter, m, l1reg, 1E-8)

  def this(maxIter: Int, m: Int, l1reg: Double, tolerance: Double = 1E-8)(
      implicit space: MutableEnumeratedCoordinateField[T, K, Double]) =
    this(maxIter, m, (_: K) => l1reg, tolerance)

  def this(maxIter: Int, m: Int, l1reg: Double)(
      implicit space: MutableEnumeratedCoordinateField[T, K, Double]) =
    this(maxIter, m, (_: K) => l1reg, 1E-8)

  def this(maxIter: Int, m: Int)(
      implicit space: MutableEnumeratedCoordinateField[T, K, Double]) =
    this(maxIter, m, (_: K) => 1.0, 1E-8)

  require(m > 0)

  import space._

  override protected def chooseDescentDirection(state: State,
                                                fn: DiffFunction[T]) = {
    val descentDir = super
      .chooseDescentDirection(state.copy(grad = state.adjustedGradient), fn)

    // The original paper requires that the descent direction be corrected to be
    // in the same directional (within the same hypercube) as the adjusted gradient for proof.
    // Although this doesn't seem to affect the outcome that much in most of cases, there are some cases
    // where the algorithm won't converge (confirmed with the author, Galen Andrew).
    val correctedDir =
      space.zipMapValues.map(descentDir, state.adjustedGradient, {
        case (d, g) => if (d * g < 0) d else 0.0
      })

    correctedDir
  }

  override protected def determineStepSize(state: State,
                                           f: DiffFunction[T],
                                           dir: T) = {
    val iter = state.iter

    val normGradInDir = {
      val possibleNorm = dir dot state.grad
//      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
//        logger.warn("Direction of positive gradient chosen!")
//        logger.warn("Direction is:" + possibleNorm)
//        Reverse the direction, clearly it's a bad idea to go up
//        dir *= -1.0
//        dir dot state.grad
//      } else {
      possibleNorm
//      }
    }

    val ff = new DiffFunction[Double] {
      def calculate(alpha: Double) = {
        val newX = takeStep(state, dir, alpha)
        val (v, newG) = f.calculate(newX)
        val (adjv, adjgrad) = adjust(newX, newG, v)
        // TODO not sure if this is quite right...

        // Technically speaking, this is not quite right.
        // dir should be (newX - state.x) according to the paper and the author.
        // However, in practice, this seems fine.
        // And interestingly the MSR reference implementation does the same thing (but they don't do wolfe condition checks.).
        adjv -> (adjgrad dot dir)
      }
    }
    val search = new BacktrackingLineSearch(state.value,
                                            shrinkStep =
                                              if (iter < 1) 0.1 else 0.5)
    val alpha =
      search.minimize(ff, if (iter < 1) .5 / norm(state.grad) else 1.0)

    alpha
  }

  // projects x to be on the same orthant as y
  // this basically requires that x'_i = x_i if sign(x_i) == sign(y_i), and 0 otherwise.

  override protected def takeStep(state: State, dir: T, stepSize: Double) = {
    val stepped = state.x + dir * stepSize
    val orthant = computeOrthant(state.x, state.adjustedGradient)
    space.zipMapValues.map(stepped, orthant, {
      case (v, ov) =>
        v * I(math.signum(v) == math.signum(ov))
    })
  }

  // Adds in the regularization stuff to the gradient
  override protected def adjust(newX: T,
                                newGrad: T,
                                newVal: Double): (Double, T) = {
    var adjValue = newVal
    val res = space.zipMapKeyValues.mapActive(
      newX,
      newGrad, {
        case (i, xv, v) =>
          val l1regValue = l1reg(i)
          require(l1regValue >= 0.0)

          if (l1regValue == 0.0) {
            v
          } else {
            adjValue += Math.abs(l1regValue * xv)
            xv match {
              case 0.0 => {
                val delta_+ = v + l1regValue
                val delta_- = v - l1regValue
                if (delta_- > 0) delta_- else if (delta_+ < 0) delta_+ else 0.0
              }
              case _ => v + math.signum(xv) * l1regValue
            }
          }
      }
    )
    adjValue -> res
  }

  private def computeOrthant(x: T, grad: T) = {
    val orth = space.zipMapValues.map(x, grad, {
      case (v, gv) =>
        if (v != 0) math.signum(v)
        else math.signum(-gv)
    })
    orth
  }
}
