/*
 * Copyright (c) 2014 Miles Sabin
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

import org.junit.Test

import test._

import ops.adjoin._

class AdjoinTests
  @Test
  def testHNil
    val adjoin = Adjoin[HNil]

    typed[HNil](adjoin(HNil))

  @Test
  def testIL
    val adjoin = Adjoin[Int :: HNil]

    typed[Int :: HNil](adjoin(1 :: HNil))

  @Test
  def testIC
    val adjoin = Adjoin[Int :+: CNil]

    typed[Int :+: CNil](adjoin(Inl(1)))

  @Test
  def testIandI
    val adjoin = Adjoin[Int :: String :: HNil]

    typed[Int :: String :: HNil](adjoin(1 :: "foo" :: HNil))

  @Test
  def testIandL
    type I = Int
    type L = String :: Symbol :: HNil
    type R = Int :: String :: Symbol :: HNil

    val adjoin = Adjoin[I :: L :: HNil]

    typed[R](adjoin(1 :: ("foo" :: 'a :: HNil) :: HNil))

  @Test
  def testLandI
    type L = Int :: String :: HNil
    type I = Symbol
    type R = Int :: String :: Symbol :: HNil

    val adjoin = Adjoin[L :: I :: HNil]

    typed[R](adjoin((1 :: "foo" :: HNil) :: 'a :: HNil))

  @Test
  def testLandL
    type L1 = Int :: String :: HNil
    type L2 = Symbol :: Char :: HNil
    type R = Int :: String :: Symbol :: Char :: HNil

    val adjoin = Adjoin[L1 :: L2 :: HNil]

    typed[R](adjoin((1 :: "foo" :: HNil) :: ('a :: 'z' :: HNil) :: HNil))

  @Test
  def testIorI
    val adjoin = Adjoin[Int :+: String :+: CNil]

    typed[Int :+: String :+: CNil](adjoin(Inl(1)))
    typed[Int :+: String :+: CNil](adjoin(Inr(Inl("foo"))))

  @Test
  def testIorC
    type I = Int
    type C = String :+: Symbol :+: CNil
    type R = Int :+: String :+: Symbol :+: CNil

    val adjoin = Adjoin[I :+: C :+: CNil]

    typed[R](adjoin(Inl(1)))
    typed[R](adjoin(Inr(Inl(Inl("foo")))))

  @Test
  def testCorI
    type C = Int :+: String :+: CNil
    type I = Symbol
    type R = Int :+: String :+: Symbol :+: CNil

    val adjoin = Adjoin[C :+: I :+: CNil]

    typed[R](adjoin(Inl(Inl(1))))
    typed[R](adjoin(Inr(Inl('x))))

  @Test
  def testCorC
    type C1 = Int :+: String :+: CNil
    type C2 = Symbol :+: Char :+: CNil
    type R = Int :+: String :+: Symbol :+: Char :+: CNil

    val adjoin = Adjoin[C1 :+: C2 :+: CNil]

    typed[R](adjoin(Inl(Inl(1))))
    typed[R](adjoin(Inr(Inl(Inl('x)))))

  @Test
  def testIandIandL
    type I1 = Int
    type I2 = String
    type L = Symbol :: HNil
    type R = Int :: String :: Symbol :: HNil

    val adjoin = Adjoin[I1 :: I2 :: L :: HNil]

    typed[R](adjoin(1 :: "foo" :: ('a :: HNil) :: HNil))

  @Test
  def testIorIorC
    type I1 = Int
    type I2 = String
    type C = Symbol :+: CNil
    type R = Int :+: String :+: Symbol :+: CNil

    val adjoin = Adjoin[I1 :+: I2 :+: C :+: CNil]

    typed[R](adjoin(Inl(1)))
    typed[R](adjoin(Inr(Inl("foo"))))

  @Test
  def testHListSyntax
    type I1 = Int
    type I2 = String
    type L = Symbol :: HNil
    type R = Int :: String :: Symbol :: HNil

    typed[R]((1 :: "foo" :: ('a :: HNil) :: HNil).adjoined)

  @Test
  def testCoproductSyntax
    type I1 = Int
    type I2 = String
    type C = Symbol :+: CNil
    type R = Int :+: String :+: Symbol :+: CNil

    val v1: I1 :+: I2 :+: C :+: CNil = Inl(1)
    val v2: I1 :+: I2 :+: C :+: CNil = Inr(Inl("foo"))

    typed[R](v1.adjoined)
    typed[R](v2.adjoined)
