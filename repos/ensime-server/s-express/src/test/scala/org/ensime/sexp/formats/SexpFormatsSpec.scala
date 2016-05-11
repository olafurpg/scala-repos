// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import org.ensime.sexp._

class SexpFormatsSpec extends FormatSpec with SexpFormats {

  val foo = SexpString("foo")
  val bar = SexpSymbol("bar")

  def assertFormat(sexp: Sexp): Unit = assertFormat(sexp, sexp)

  "SexpFormats" should "support SexpAtoms" in {
    assertFormat(SexpNil)
    assertFormat(SexpPosInf)
    assertFormat(SexpNegInf)
    assertFormat(SexpNaN)
    assertFormat(SexpNumber(1))
    assertFormat(SexpString("hello"))
    assertFormat(SexpSymbol("hello"))
  }

  it should "support SexpList" in {
    assertFormat(SexpList(foo, bar))
  }

  it should "support SexpCons" in {
    assertFormat(SexpCons(foo, bar))
  }
}

