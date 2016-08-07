/*
 * Copyright (c) 2011-15 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless
package ops

object nat {

  /**
    * Type class witnessing that `B` is the predecessor of `A`.
    *
    * @author Miles Sabin
    */
  trait Pred[A <: Nat] extends Serializable { type Out <: Nat }

  object Pred {
    def apply[A <: Nat](implicit pred: Pred[A]): Aux[A, pred.Out] = pred

    type Aux[A <: Nat, B <: Nat] = Pred[A] { type Out = B }

    implicit def pred[B <: Nat]: Aux[Succ[B], B] = new Pred[Succ[B]] {
      type Out = B
    }
  }

  /**
    * Type class witnessing that `C` is the sum of `A` and `B`.
    *
    * @author Miles Sabin
    */
  trait Sum[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Sum {
    def apply[A <: Nat, B <: Nat](
        implicit sum: Sum[A, B]): Aux[A, B, sum.Out] = sum

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Sum[A, B] { type Out = C }

    implicit def sum1[B <: Nat]: Aux[_0, B, B] = new Sum[_0, B] {
      type Out = B
    }
    implicit def sum2[A <: Nat, B <: Nat](
        implicit sum: Sum[A, Succ[B]]): Aux[Succ[A], B, sum.Out] =
      new Sum[Succ[A], B] { type Out = sum.Out }
  }

  /**
    * Type class witnessing that `C` is the difference of `A` and `B`.
    *
    * @author Miles Sabin
    */
  trait Diff[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Diff {
    def apply[A <: Nat, B <: Nat](
        implicit diff: Diff[A, B]): Aux[A, B, diff.Out] = diff

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Diff[A, B] { type Out = C }

    implicit def diff1[A <: Nat]: Aux[A, _0, A] = new Diff[A, _0] {
      type Out = A
    }
    implicit def diff2[A <: Nat, B <: Nat](
        implicit diff: Diff[A, B]): Aux[Succ[A], Succ[B], diff.Out] =
      new Diff[Succ[A], Succ[B]] { type Out = diff.Out }
  }

  /**
    * Type class witnessing that `C` is the product of `A` and `B`.
    *
    * @author Miles Sabin
    */
  trait Prod[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Prod {
    def apply[A <: Nat, B <: Nat](
        implicit prod: Prod[A, B]): Aux[A, B, prod.Out] = prod

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Prod[A, B] { type Out = C }

    implicit def prod1[B <: Nat]: Aux[_0, B, _0] = new Prod[_0, B] {
      type Out = _0
    }
    implicit def prod2[A <: Nat, B <: Nat, C <: Nat](
        implicit prod: Prod.Aux[A, B, C],
        sum: Sum[B, C]): Aux[Succ[A], B, sum.Out] = new Prod[Succ[A], B] {
      type Out = sum.Out
    }
  }

  /**
    * Type class witnessing that `Out` is the quotient of `A` and `B`.
    *
    * @author Tom Switzer
    */
  trait Div[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Div {
    def apply[A <: Nat, B <: Nat](
        implicit div: Div[A, B]): Aux[A, B, div.Out] = div

    import LT._

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Div[A, B] { type Out = C }

    implicit def div1[A <: Nat]: Aux[_0, A, _0] = new Div[_0, A] {
      type Out = _0
    }

    implicit def div2[A <: Nat, B <: Nat](implicit lt: A < B): Aux[A, B, _0] =
      new Div[A, B] { type Out = _0 }

    implicit def div3[A <: Nat, B <: Nat, C <: Nat, D <: Nat](
        implicit diff: Diff.Aux[Succ[A], B, C],
        div: Div.Aux[C, B, D]): Aux[Succ[A], B, Succ[D]] =
      new Div[Succ[A], B] { type Out = Succ[D] }
  }

  /**
    * Typeclass witnessing that `Out` is `A` mod `B`.
    *
    * @author Tom Switzer
    */
  trait Mod[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Mod {
    def apply[A <: Nat, B <: Nat](
        implicit mod: Mod[A, B]): Aux[A, B, mod.Out] = mod

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Mod[A, B] { type Out = C }

    implicit def modAux[A <: Nat, B <: Nat, C <: Nat, D <: Nat, E <: Nat](
        implicit div: Div.Aux[A, B, C],
        prod: Prod.Aux[C, B, D],
        diff: Diff.Aux[A, D, E]): Aux[A, B, E] =
      new Mod[A, B] { type Out = E }
  }

  /**
    * Type class witnessing that `A` is less than `B`.
    *
    * @author Miles Sabin
    */
  trait LT[A <: Nat, B <: Nat] extends Serializable

  object LT extends LT0 {
    def apply[A <: Nat, B <: Nat](implicit lt: A < B): LT[A, B] = lt

    implicit def lt1[B <: Nat] = new <[_0, Succ[B]] {}
    implicit def lt2[A <: Nat, B <: Nat](implicit lt: A < B) =
      new <[Succ[A], Succ[B]] {}
  }

  trait LT0 {
    type <[A <: Nat, B <: Nat] = LT[A, B]

    implicit def lt3[A <: Nat] = new <[A, Succ[A]] {}
  }

  /**
    * Type class witnessing that `A` is less than or equal to `B`.
    *
    * @author Miles Sabin
    */
  trait LTEq[A <: Nat, B <: Nat] extends Serializable

  object LTEq extends LTEq0 {
    def apply[A <: Nat, B <: Nat](implicit lteq: A <= B): LTEq[A, B] = lteq

    implicit def ltEq1[A <: Nat] = new <=[A, A] {}
    implicit def ltEq2[A <: Nat] = new <=[A, Succ[A]] {}
  }

  trait LTEq0 {
    type <=[A <: Nat, B <: Nat] = LTEq[A, B]

    implicit def ltEq3[B <: Nat] = new <=[_0, B] {}
    implicit def ltEq4[A <: Nat, B <: Nat](implicit lteq: A <= B) =
      new <=[Succ[A], Succ[B]] {}
  }

  /**
    * Type class witnessing that `A` is greater than `B`.
    *
    * @author ryoppy
    */
  type GT[A <: Nat, B <: Nat] = LT[B, A]
  object GT {
    def apply[A <: Nat, B <: Nat](implicit gt: GT[A, B]): GT[A, B] = gt
    type >[A <: Nat, B <: Nat] = GT[A, B]
  }

  /**
    * Type class witnessing that `A` is greater than or equal to `B`.
    *
    * @author ryoppy
    */
  type GTEq[A <: Nat, B <: Nat] = LTEq[B, A]
  object GTEq {
    def apply[A <: Nat, B <: Nat](implicit gteq: GTEq[A, B]): GTEq[A, B] = gteq
    type >=[A <: Nat, B <: Nat] = GTEq[A, B]
  }

  /**
    * Type class witnessing that `Out` is `A` min `B`.
    *
    * @author George Leontiev
    */
  trait Min[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Min {
    def apply[A <: Nat, B <: Nat](
        implicit min: Min[A, B]): Aux[A, B, min.Out] = min

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Min[A, B] { type Out = C }

    implicit def minAux0[A <: Nat, B <: Nat, C <: Nat](
        implicit lteq: LTEq[A, B]): Aux[A, B, A] = new Min[A, B] {
      type Out = A
    }
    implicit def minAux1[A <: Nat, B <: Nat, C <: Nat](
        implicit lteq: LT[B, A]): Aux[A, B, B] = new Min[A, B] { type Out = B }
  }

  /**
    * Type class witnessing that `Out` is `A` max `B`.
    *
    * @author Alexander Konovalov
    */
  trait Max[A <: Nat, B <: Nat] extends Serializable { type Out <: Nat }

  object Max {
    def apply[A <: Nat, B <: Nat](
        implicit max: Max[A, B]): Aux[A, B, max.Out] = max

    type Aux[A <: Nat, B <: Nat, C <: Nat] = Max[A, B] { type Out = C }

    implicit def maxAux0[A <: Nat, B <: Nat, C <: Nat](
        implicit lteq: LTEq[A, B]): Aux[A, B, B] = new Max[A, B] {
      type Out = B
    }
    implicit def maxAux1[A <: Nat, B <: Nat, C <: Nat](
        implicit lteq: LT[B, A]): Aux[A, B, A] = new Max[A, B] { type Out = A }
  }

  /**
    * Type class witnessing that `Out` is `X` raised to the power `N`.
    *
    * @author George Leontiev
    */
  trait Pow[N <: Nat, X <: Nat] extends Serializable { type Out <: Nat }

  object Pow {
    def apply[A <: Nat, B <: Nat](
        implicit pow: Pow[A, B]): Aux[A, B, pow.Out] = pow

    import shapeless.nat._1

    type Aux[N <: Nat, X <: Nat, Z <: Nat] = Pow[N, X] { type Out = Z }

    implicit def pow1[A <: Nat]: Aux[Succ[A], _0, _0] = new Pow[Succ[A], _0] {
      type Out = _0
    }
    implicit def pow2[A <: Nat]: Aux[_0, Succ[A], _1] = new Pow[_0, Succ[A]] {
      type Out = _1
    }
    implicit def pow3[N <: Nat, X <: Nat, Z <: Nat, Y <: Nat](
        implicit ev: Pow.Aux[N, X, Z],
        ev2: Prod.Aux[Z, X, Y]): Aux[Succ[N], X, Y] = new Pow[Succ[N], X] {
      type Out = Y
    }
  }

  /**
    * Type class witnessing that `Out` is range `A` to `B`.
    *
    * @author Andreas Koestler
    */
  trait Range[A <: Nat, B <: Nat] extends DepFn0 with Serializable {
    type Out <: HList
  }

  object Range {
    def apply[A <: Nat, B <: Nat](
        implicit range: Range[A, B]): Aux[A, B, range.Out] = range

    type Aux[A <: Nat, B <: Nat, Out0 <: HList] = Range[A, B] {
      type Out = Out0
    }

    import shapeless.ops.hlist._

    implicit def range1[A <: Nat]: Aux[A, A, HNil] = new Range[A, A] {
      type Out = HNil

      def apply(): Out = HNil
    }

    implicit def range2[A <: Nat, B <: Nat, L <: HList, LO <: HList](
        implicit w: Witness.Aux[B],
        r: Range.Aux[A, B, L],
        prep: Prepend.Aux[L, B :: HNil, LO]): Aux[A, Succ[B], LO] =
      new Range[A, Succ[B]] {
        type Out = LO

        def apply(): Out = r() :+ w.value
      }
  }

  /**
    * Type class implementing Euclidean algorithm for calculating the GCD
    *
    * @author Andreas Koestler
    */
  trait LowPriorityGCD {
    implicit def defaultCase[A <: Nat, B <: Nat, T <: Nat](
        implicit mod: Mod.Aux[A, B, T],
        gcd: GCD[B, T]): GCD.Aux[A, B, gcd.Out] = new GCD[A, B] {
      type Out = gcd.Out
    }
  }
  trait GCD[A <: Nat, B <: Nat] extends Serializable {
    type Out <: Nat
  }

  object GCD extends LowPriorityGCD {
    def apply[A <: Nat, B <: Nat](
        implicit gcd: GCD[A, B]): Aux[A, B, gcd.Out] = gcd

    type Aux[A <: Nat, B <: Nat, Out0 <: Nat] = GCD[A, B] { type Out = Out0 }

    implicit def terminationCase[A <: Nat]: Aux[A, _0, A] = new GCD[A, _0] {
      type Out = A
    }
  }

  /**
    * Type class for calculating the Least Common Multiple
    *
    * @author Andreas Koestler
    */
  trait LCM[A <: Nat, B <: Nat] extends Serializable {
    type Out <: Nat
  }

  object LCM {
    def apply[A <: Nat, B <: Nat](
        implicit lcm: LCM[A, B]): Aux[A, B, lcm.Out] = lcm

    type Aux[A <: Nat, B <: Nat, Out0 <: Nat] = LCM[A, B] { type Out = Out0 }

    implicit def lcm[A <: Nat, B <: Nat, M <: Nat, N <: Nat, Res <: Nat](
        implicit prod: Prod.Aux[A, B, M],
        gcd: GCD.Aux[A, B, N],
        div: Div[M, N]): Aux[A, B, div.Out] = new LCM[A, B] {
      type Out = div.Out
    }
  }

  /**
    * Type class supporting conversion of type-level Nats to value level Ints.
    *
    * @author Miles Sabin
    */
  trait ToInt[N <: Nat] extends Serializable {
    def apply(): Int
  }

  object ToInt {
    def apply[N <: Nat](implicit toInt: ToInt[N]): ToInt[N] = toInt

    implicit val toInt0 = new ToInt[_0] {
      def apply() = 0
    }
    implicit def toIntSucc[N <: Nat](implicit toIntN: ToInt[N]) =
      new ToInt[Succ[N]] {
        def apply() = toIntN() + 1
      }
  }
}
