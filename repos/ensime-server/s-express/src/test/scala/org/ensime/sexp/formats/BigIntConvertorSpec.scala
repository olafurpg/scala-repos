// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import BigIntConvertor._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.immutable.BitSet
import org.ensime.util.EnsimeSpec

class BigIntConvertorSpec extends EnsimeSpec {
  private val examples = List(
    BitSet() -> BigInt(0),
    BitSet(0) -> BigInt(1),
    BitSet(1) -> BigInt(2),
    BitSet(64) -> BigInt("18446744073709551616"),
    BitSet(0, 64) -> BigInt("18446744073709551617"),
    BitSet(1, 64) -> BigInt("18446744073709551618")
  )

  "BigIntConvertor" should "convert basic BigSet to BitInt" in {
    examples foreach {
      case (bitset, bigint) => fromBitSet(bitset) should ===(bigint)
    }
  }

  it should "convert basic BigInt to BitSet" in {
    examples foreach {
      case (bitset, bigint) => toBitSet(bigint) should ===(bitset)
    }
  }
}

class BigIntConvertorCheck extends EnsimeSpec with GeneratorDrivenPropertyChecks {

  def positiveIntStream: Arbitrary[Stream[Int]] = Arbitrary {
    Gen.containerOf[Stream, Int](Gen.chooseNum(0, 2 * Short.MaxValue))
  }

  implicit def arbitraryBitSet: Arbitrary[BitSet] = Arbitrary {
    for (seq <- positiveIntStream.arbitrary) yield BitSet(seq: _*)
  }

  "BigIntConvertor" should "round-trip BigInt <=> BitSet" in {
    forAll { (bigint: BigInt) =>
      whenever(bigint >= 0) {
        // the exact rules for which negative numbers are allowed
        // seems to be quite complex, but certainly it is sometimes
        // valid.
        fromBitSet(toBitSet(bigint)) should ===(bigint)
      }
    }
  }

  it should "round-trip BitSet <=> BigInt" in {
    forAll { (bitset: BitSet) =>
      toBitSet(fromBitSet(bitset)) should ===(bitset)
    }
  }
}
