/*
 * Copyright (c) 2011 Miles Sabin 
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

package shapeless.examples

object StackOverflow2
  // http://stackoverflow.com/questions/8270526

  import shapeless._
  import ops.function._

  sealed abstract class A { def eval(): A }
  case class A0() extends A { def eval() = this }
  case class A1(a: A) extends A { def eval() = this }
  case class A2(a: A, b: A) extends A { def eval() = this }

  case class ApplyA[C, L <: HList, HF](c: C, l: L)(
      implicit fntp: FnToProduct.Aux[C, HF], ev: HF <:< (L => A))
      extends A
    def eval(): A = fntp(c)(l)

  val a: A = A0()

  val a0 = ApplyA(A0.apply _, HNil)
  val a1 = ApplyA(A1.apply _, a :: HNil)
  val a2 = ApplyA(A2.apply _, a :: a :: HNil)

object StackOverflow3
  // http://stackoverflow.com/questions/8681491

  import shapeless._
  import poly._
  import ops.hlist.{Mapped, Mapper}
  import syntax.std.function._

  case class Input[T](value: T)

  object value extends (Input ~> Id)
    def apply[T](i: Input[T]) = i.value

  class Preprocessor[In <: HList, Out <: HList, R](
      ctor: Out => R)(implicit mapped: Mapped.Aux[Out, Input, In],
                      mapper: Mapper.Aux[value.type, In, Out])
    def apply(in: In) = ctor(in map value)

  case class Foo(input1: Int, input2: String)

  object FooBuilder extends Preprocessor((Foo.apply _).toProduct)

  val foo = FooBuilder(Input(23) :: Input("foo") :: HNil)

  case class Bar(input1: Int, input2: String, input3: Boolean)

  object BarBuilder extends Preprocessor((Bar.apply _).toProduct)

  val bar = BarBuilder(Input(23) :: Input("foo") :: Input(true) :: HNil)

object StackOverflow4 extends App
  // http://stackoverflow.com/questions/10216278

  import shapeless._
  import ops.function._
  import syntax.std.function._

  def fun(x: Int) = x
  def fun1(x: Int, y: Int) = x
  def fun2(x: Int, foo: Map[Int, String], bar: Seq[Seq[Int]]) = x

  def wrap_fun[F, T <: HList, R](
      f: F)(implicit fntp: FnToProduct.Aux[F, (Int :: T) => R],
            fnfp: FnFromProduct.Aux[(Int :: T) => R, F]): F =
    ((x: Int :: T) => f.toProduct(x.head * 2 :: x.tail)).fromProduct

  val f1 = wrap_fun(fun _)
  val f2 = wrap_fun(fun1 _)
  val f3 = wrap_fun(fun2 _)

  assert(f1(2) == 4)
  assert(f2(2, 4) == 4)
  assert(f3(2, Map(), Seq()) == 4)
