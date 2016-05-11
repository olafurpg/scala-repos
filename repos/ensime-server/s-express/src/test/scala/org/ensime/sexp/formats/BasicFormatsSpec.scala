// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import org.ensime.sexp._

class BasicFormatsSpec extends FormatSpec with BasicFormats {

  "BasicFormats" should "support Int" in {
    assertFormat(13, SexpNumber(13))
    assertFormat(-1, SexpNumber(-1))
    assertFormat(0, SexpNumber(0))
    assertFormat(Int.MaxValue, SexpNumber(Int.MaxValue))
    assertFormat(Int.MinValue, SexpNumber(Int.MinValue))
  }

  it should "support Long" in {
    assertFormat(13L, SexpNumber(13))
    assertFormat(-1L, SexpNumber(-1))
    assertFormat(0L, SexpNumber(0))
    assertFormat(Long.MaxValue, SexpNumber(Long.MaxValue))
    assertFormat(Long.MinValue, SexpNumber(Long.MinValue))
  }

  it should "support Float" in {
    assertFormat(13.0f, SexpNumber(13.0f))
    assertFormat(-1.0f, SexpNumber(-1.0f))
    assertFormat(0.0f, SexpNumber(0.0f))
    assertFormat(Float.MaxValue, SexpNumber(Float.MaxValue))
    //assertFormat(Float.MinValue, SexpNumber(Float.MinValue)) // implicit widening?
    assertFormat(Float.NegativeInfinity, SexpNegInf)
    assertFormat(Float.PositiveInfinity, SexpPosInf)

    // remember NaN != NaN
    Float.NaN.toSexp should ===(SexpNaN)
    SexpNaN.convertTo[Float].isNaN shouldBe true
  }

  it should "support Double" in {
    assertFormat(13.0d, SexpNumber(13.0d))
    assertFormat(-1.0d, SexpNumber(-1.0d))
    assertFormat(0.0d, SexpNumber(0.0d))
    assertFormat(Double.MaxValue, SexpNumber(Double.MaxValue))
    assertFormat(Double.MinValue, SexpNumber(Double.MinValue))
    assertFormat(Double.NegativeInfinity, SexpNegInf)
    assertFormat(Double.PositiveInfinity, SexpPosInf)

    // remember NaN != NaN
    Double.NaN.toSexp should ===(SexpNaN)
    SexpNaN.convertTo[Double].isNaN shouldBe true
  }

  it should "support Boolean" in {
    assertFormat(true, SexpSymbol("t"))
    assertFormat(false, SexpNil)
  }

  it should "support Char" in {
    assertFormat('t', SexpChar('t'))
  }

  it should "support Unit" in {
    assertFormat((), SexpNil)
  }

  it should "support Symbol" in {
    assertFormat('blah, SexpString("blah"))
  }
}
