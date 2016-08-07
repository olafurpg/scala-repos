package breeze.linalg
/*
 Copyright 2012 David Hall
 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 http://www.apache.org/licenses/LICENSE-2.0
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.generic.UFunc
import breeze.linalg.operators.{OpAdd, OpType}
import breeze.math.MutableOptimizationSpace.SparseFieldOptimizationSpace
import breeze.math.{Field, Complex}
import breeze.storage.Zero
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import scala.reflect.ClassTag

@RunWith(classOf[JUnitRunner])
class CSCMatrixTest extends FunSuite with Checkers {
  test("Multiply") {
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val ad = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = CSCMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0), (12.0, 0.0, 5.0))
    val bd = DenseMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0), (12.0, 0.0, 5.0))
    val c = DenseVector(6.0, 2.0, 3.0)
    assert(
        (a * b: CSCMatrix[Double]) === CSCMatrix((37.0, -8.0, 25.0),
                                                 (85.0, -23.0, 67.0)))
    assert(
        (a * bd: DenseMatrix[Double]) === DenseMatrix((37.0, -8.0, 25.0),
                                                      (85.0, -23.0, 67.0)))
    assert(
        (ad * b: DenseMatrix[Double]) === DenseMatrix((37.0, -8.0, 25.0),
                                                      (85.0, -23.0, 67.0)))
    assert(a * c === DenseVector(19.0, 52.0))
    assert(b * c === DenseVector(62.0, -21.0, 87.0))

//    assert(b.t * c === DenseVector(72.0, -18.0, 65.0))
//    assert(a.t * DenseVector(4.0, 3.0) === DenseVector(16.0, 23.0, 30.0))

    // should be dense
//    val x = a * a.t
//    assert(x === DenseMatrix((14.0,32.0),(32.0,77.0)))

    // should be dense
//    val y = a.t * a
//    assert(y === DenseMatrix((17.0,22.0,27.0),(22.0,29.0,36.0),(27.0,36.0,45.0)))

//    val z : DenseMatrix[Double] = b * (b + 1.0)
//    assert(z === DenseMatrix((164.0,5.0,107.0),(-5.0,10.0,-27.0),(161.0,-7.0,138.0)))
  }

  test("Multiply Int") {
    val a = CSCMatrix((1, 2, 3), (4, 5, 6))
    val b = CSCMatrix((7, -2, 8), (-3, -3, 1), (12, 0, 5))
    val bd = DenseMatrix((7, -2, 8), (-3, -3, 1), (12, 0, 5))
    val c = DenseVector(6, 2, 3)
    val cs = SparseVector(3)((1, 2))
    assert(a * b === CSCMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * bd === DenseMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * c === DenseVector(19, 52))
    assert(b * c === DenseVector(62, -21, 87))
    assert(a * cs === SparseVector(4, 10))
    assert(b * cs === SparseVector(3)((0, -4), (1, -6)))

//    assert(b.t * c === DenseVector(72, -18, 65))
//    assert(a.t * DenseVector(4, 3) === DenseVector(16, 23, 30))

    // should be dense
//    val x = a * a.t
//    assert(x === DenseMatrix((14,32),(32,77)))

    // should be dense
//    val y = a.t * a
//    assert(y === DenseMatrix((17,22,27),(22,29,36),(27,36,45)))

//    val z : DenseMatrix[Double] = b * (b + 1.0)
//    assert(z === DenseMatrix((164,5,107),(-5,10,-27),(161,-7,138)))
  }

  test("Multiply Complex") {

    val a = CSCMatrix((Complex(1, 1), Complex(2, 2), Complex(3, 3)),
                      (Complex(4, 4), Complex(5, 5), Complex(6, 6)))
    val b = CSCMatrix((Complex(7, 7), Complex(-2, -2), Complex(8, 8)),
                      (Complex(-3, -3), Complex(-3, -3), Complex(1, 1)),
                      (Complex(12, 12), Complex(0, 0), Complex(5, 5)))
    val c = DenseVector(Complex(6, 0), Complex(2, 0), Complex(3, 0))
    val cs = SparseVector(Complex(6, 0), Complex(2, 0), Complex(3, 0))
    val value: CSCMatrix[Complex] = a * b
    assert(
        value === CSCMatrix(
            (Complex(0, 74), Complex(0, -16), Complex(0, 50)),
            (Complex(0, 170), Complex(0, -46), Complex(0, 134))))
    assert(
        b * c === DenseVector(Complex(62, 62),
                              Complex(-21, -21),
                              Complex(87, 87)))
    assert(
        b * cs === DenseVector(Complex(62, 62),
                               Complex(-21, -21),
                               Complex(87, 87)))
    assert(
        b.t * c === DenseVector(Complex(72, -72),
                                Complex(-18, 18),
                                Complex(65, -65)))
  }

  test("Transpose") {
    val a = CSCMatrix.zeros[Int](2, 3)
    a(0, 0) = 1
    a(1, 2) = 2

    val expected = CSCMatrix.zeros[Int](3, 2)
    expected(0, 0) = 1
    expected(2, 1) = 2

    assert(a.t === expected)
  }

  test("Transpose Complex") {
    val a = CSCMatrix.zeros[Complex](2, 3)
    a(0, 0) = Complex(1, 1)
    a(1, 2) = Complex(-2, -2)

    val expected = CSCMatrix.zeros[Complex](3, 2)
    expected(0, 0) = Complex(1, -1)
    expected(2, 1) = Complex(-2, 2)

    assert(a.t === expected)
  }

  test("Generic CSC ops") {
    // mostly for coverage
    val a = CSCMatrix.create[String](1, 1, Array("SSS"))
    intercept[IndexOutOfBoundsException] {
      a(3, 3) = ":("
      assert(false, "Shouldn't be here!")
    }
    assert(a(0, 0) === "SSS")
    intercept[IndexOutOfBoundsException] {
      a(3, 3)
      assert(false, "Shouldn't be here!")
    }
    a(0, 0) = ":("
    assert(a(0, 0) === ":(")
  }

  test("Builder, simple") {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(1, 1, 2.0)
  }

  test("Builder, full") {
    val builder = new CSCMatrix.Builder[Double](2, 3)
    builder.add(0, 1, 2.0)
    builder.add(1, 1, 5.0)
    builder.add(0, 2, 3.0)
    builder.add(1, 0, 4.0)
    builder.add(1, 2, 6.0)
    builder.add(0, 0, 1.0)
    val cs = builder.result()
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    assert(cs === a)
  }

  test("Builder, repeated full") {
    val builder = new CSCMatrix.Builder[Double](2, 3)
    builder.add(0, 1, 2.0)
    builder.add(1, 2, 3.0)
    builder.add(1, 1, 5.0)
    builder.add(0, 2, 3.0)
    builder.add(1, 0, 4.0)
    builder.add(1, 2, 3.0)
    builder.add(0, 0, 1.0)
    val cs = builder.result()
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    assert(cs === a)
  }

  test("MapValues") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))

    val b1: CSCMatrix[Int] = a.mapValues(_ + 1)
    assert(b1 === CSCMatrix((2, 1, 1), (3, 4, 0)))

    val b2: CSCMatrix[Double] = a.mapValues(_ + 1.0)
    assert(b2 === CSCMatrix((2.0, 1.0, 1.0), (3.0, 4.0, 0.0)))
  }

  test("addition/subtraction") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))
    val b: CSCMatrix[Int] = CSCMatrix((0, 1, 0), (2, 3, -1))
    assert(a + b === CSCMatrix((1, 1, 0), (4, 6, -2)))
    assert(a - b === CSCMatrix((1, -1, 0), (0, 0, 0)))
  }

  test("addition/subtraction csc/dm") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))
    val b: DenseMatrix[Int] = DenseMatrix((0, 1, 0), (2, 3, -1))
    assert(a + b === DenseMatrix((1, 1, 0), (4, 6, -2)))
    assert(a - b === DenseMatrix((1, -1, 0), (0, 0, 0)))
    assert(b - a === -DenseMatrix((1, -1, 0), (0, 0, 0)))
  }

  test("inplace addition/subtraction") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))
    val b: CSCMatrix[Int] = CSCMatrix((0, 1, 0), (2, 3, -1))
    a += b
    assert(a === CSCMatrix((1, 1, 0), (4, 6, -2)))
    assert(a.activeSize === 5)
    a -= b
    a -= b
    assert(a === CSCMatrix((1, -1, 0), (0, 0, 0)))
    assert(a.activeSize === 2)
  }

  test("inplace set dm/csc") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))
    val b: DenseMatrix[Int] = DenseMatrix((0, 1, 0), (2, 3, -1))
    b := a
    assert(a == b)
  }

  test("InPlace Ops") {
    var a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = CSCMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0))

    a :*= b
    assert(a === CSCMatrix((7.0, -4.0, 24.0), (-12.0, -15.0, 6.0)))

    a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    a :/= b
    assert(
        a === CSCMatrix((1.0 / 7.0, -1.0, 3.0 / 8.0),
                        (4.0 / (-3.0), 5.0 / (-3.0), 6.0)))
  }

  test("csc scalar \"bad\" ops") {
    val a: CSCMatrix[Int] = CSCMatrix((1, 0, 0), (2, 3, -1))
    assert(a :/ 3 === CSCMatrix((0, 0, 0), (0, 1, 0)))

    val b: CSCMatrix[Complex] =
      CSCMatrix((Complex(1, 0), Complex(0, 0), Complex(0, 0)),
                (Complex(2, 0), Complex(3, 0), Complex(-1, 0)))
    assert(
        b :/ Complex(3, 0) === CSCMatrix(
            (Complex(1.0 / 3.0, 0), Complex(0, 0), Complex(0, 0)),
            (Complex(2.0 / 3.0, 0), Complex(1, 0), Complex(-1.0 / 3.0, 0))))
  }

  test("csc scalar \"bad\" pow ops") {
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = CSCMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0))

    assert(a :^ b === a.toDense :^ b.toDense)
    val ac = convert(a, Complex)
    val bc = convert(b, Complex)
    assert(ac :^ bc === ac.toDense :^ bc.toDense)
  }

  test("csc scalar \"bad\" mod ops") {
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = CSCMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0))

    assert(a :% b === a.toDense :% b.toDense)

    val ac = convert(a, Complex)
    val bc = convert(b, Complex)
    assert(ac :% bc === ac.toDense :% bc.toDense)
  }

  test("flatten") {
    val a = CSCMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = CSCMatrix.zeros[Double](3, 2)
    b(0, 1) = 1.0; b(2, 1) = 3.0
    val z = CSCMatrix.zeros[Double](5, 3)
    assert(a.flatten() === SparseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    assert(z.flatten() === SparseVector.zeros[Double](15))
    assert(b.flatten() === SparseVector(6)((1, 1.0), (5, 3.0)))
  }

  test("CSCxCSC: OpAddInPlace2:Field") {
    def testAddInPlace[T: Field: Zero: ClassTag](a: CSCMatrix[T],
                                                 b: CSCMatrix[T]) = {
      val optspace = SparseFieldOptimizationSpace.sparseOptSpace[T]
      import optspace._
      a += b
    }
    val cscA = CSCMatrix.zeros[Double](3, 4)
    val cscB = CSCMatrix.zeros[Double](3, 4)
    cscB(1, 1) = 1.3
    cscB(0, 0) = 1.0
    cscB(2, 3) = 1.8
    cscB(2, 0) = 1.6
    testAddInPlace[Double](cscA, cscB)
    assert(cscA === cscB)
    testAddInPlace[Double](cscA, cscB)
    assert(cscA === cscB * 2.0)
    testAddInPlace[Double](cscA, CSCMatrix.zeros[Double](3, 4))
    assert(cscA === cscB * 2.0)
  }

  test("CSCxCSC: OpSubInPlace2:Field") {
    def testSubInPlace[T: Field: Zero: ClassTag](a: CSCMatrix[T],
                                                 b: CSCMatrix[T]) = {
      val optspace = SparseFieldOptimizationSpace.sparseOptSpace[T]
      import optspace._
      a -= b
    }
    val cscA = CSCMatrix.zeros[Double](3, 4)
    val cscB = CSCMatrix.zeros[Double](3, 4)
    cscB(1, 1) = 1.3
    cscB(0, 0) = 1.0
    cscB(2, 3) = 1.8
    cscB(2, 0) = 1.6
    testSubInPlace[Double](cscA, cscB)
    assert(cscA === cscB * -1.0)
    testSubInPlace[Double](cscA, cscB)
    assert(cscA === cscB * -2.0)
    testSubInPlace[Double](cscA, CSCMatrix.zeros[Double](3, 4))
    assert(cscA === cscB * -2.0)
  }
  test("CSCxCSC: OpMulScalarInPlace2:Field") {
    def testMulScalarInPlace[T: Field: Zero: ClassTag](a: CSCMatrix[T],
                                                       b: CSCMatrix[T]) = {
      val optspace = SparseFieldOptimizationSpace.sparseOptSpace[T]
      import optspace._
      a *= b
    }
    val cscA = CSCMatrix.zeros[Double](3, 4)
    val cscB = CSCMatrix.zeros[Double](3, 4)
    cscB(1, 1) = 1.3
    cscB(0, 0) = 1.0
    cscB(2, 3) = 1.8
    cscB(2, 0) = 1.6
    testMulScalarInPlace[Double](cscA, cscB)
    assert(cscA === cscA)
    cscA(1, 1) = 2.0
    cscA(0, 0) = 2.0
    cscA(1, 0) = 2.0
    testMulScalarInPlace[Double](cscA, cscB)
    val cscR = CSCMatrix.zeros[Double](3, 4)
    cscR(1, 1) = 2.6
    cscR(0, 0) = 2.0
    assert(cscA === cscR)
    testMulScalarInPlace[Double](cscA, CSCMatrix.zeros[Double](3, 4))
    assert(cscA === CSCMatrix.zeros[Double](3, 4))
  }

  test("CSCxCSC: OpSetInPlace:Scalar:Field") {
    def testSetInPlace[T: Field: Zero: ClassTag](a: CSCMatrix[T], b: T) = {
      val optspace = SparseFieldOptimizationSpace.sparseOptSpace[T]
      import optspace._
      a := b
    }
    val cscA = CSCMatrix.zeros[Double](3, 4)
    val b = 4
    testSetInPlace[Double](cscA, b)
    assert(cscA === CSCMatrix.fill(3, 4)(b))
    testSetInPlace[Double](cscA, 0)
    assert(cscA === CSCMatrix.zeros[Double](3, 4))
  }
  test("ZipMapVals Test") {
    def testZipMap[T: Field: Zero: ClassTag](a: CSCMatrix[T],
                                             b: CSCMatrix[T]): CSCMatrix[T] = {
      val f = implicitly[Field[T]]
      val optspace = SparseFieldOptimizationSpace.sparseOptSpace[T]
      import optspace._
      val addMapFn = (t1: T, t2: T) => f.+(t1, t2)

      zipMapValuesM.map(a, b, addMapFn)
    }
    val cscA = CSCMatrix.zeros[Double](3, 4)
    val cscB = CSCMatrix.zeros[Double](3, 4)
    cscB(1, 1) = 1.3
    cscB(0, 0) = 1.0
    cscB(2, 3) = 1.8
    cscB(2, 0) = 1.6
    val cscR = testZipMap(cscA, cscB)
    assert(cscR === cscB)
    val cscR1 = testZipMap(cscR, cscB)
    assert(cscR1 === cscB * 2.0)
    cscR(1, 0) = 1.1
    cscB(0, 1) = 1.2
    val cscR2 = testZipMap(cscR, cscB)
    val cscR3 = cscR * 2.0
    cscR3(1, 0) = 1.1
    cscR3(0, 1) = 1.2
    assert(cscR2 === cscR3)
    val cscR4 = testZipMap(cscB, cscA)
    assert(cscR4 === cscB)
  }

  test("axpy") {
    val a = CSCMatrix((1, 0, 0), (2, 3, -1))
    val b = CSCMatrix((0, 1, 0), (2, 3, -1))
    axpy(2, b, a)
    assert(a === CSCMatrix((1, 2, 0), (6, 9, -3)))
  }

  test("#344") {
    val builder = new CSCMatrix.Builder[Double](rows = 10, cols = 10)
    builder.add(0, 0, 1.0)
    builder.add(1, 0, 1.0)

    val a = builder.result

    a.update(0, 0, 0.0)
    assert(a.t.rows === a.cols)
  }

  test("#348") {
    val a = DenseMatrix((2, 2), (3, 3))
    val b = CSCMatrix((2, 2), (3, 3))
    assert(a + b === a + b.toDense)
  }

  test("#313") {
    val builder = new CSCMatrix.Builder[Double](rows = -1, cols = -1)
    builder.add(0, 0, 1.0)
    builder.add(1, 0, 1.0)

    val a = builder.result

    assert(a.rows == 2)
    assert(a.cols == 1)
  }

  test("#479") {
    val m1: Matrix[Double] = new CSCMatrix[Double](Array(1.0, 1, 1),
                                                   3,
                                                   3,
                                                   Array(0, 1, 2, 3),
                                                   Array(0, 1, 2))
    val m2: Matrix[Double] = new CSCMatrix[Double](Array(1.0, 2, 2, 4),
                                                   3,
                                                   3,
                                                   Array(0, 0, 2, 4),
                                                   Array(1, 2, 1, 2))

    val sum = (m1 + m2).asInstanceOf[CSCMatrix[Double]]
    require(sum.colPtrs.last == sum.rowIndices.length,
            s"${sum.colPtrs.last} not equal to ${sum.rowIndices.length}")
  }

  test("CSCMatrix Solve") {
    val r2: DenseVector[Double] =
      CSCMatrix((1.0, 3.0, 4.0), (2.0, 0.0, 6.0)) \ DenseVector(1.0, 3.0)
    import breeze.numerics.inf
    assert(
        norm(r2 - DenseVector(0.1813186813186811,
                              -0.3131868131868131,
                              0.43956043956043944),
             inf) < 1E-5)
  }
}
