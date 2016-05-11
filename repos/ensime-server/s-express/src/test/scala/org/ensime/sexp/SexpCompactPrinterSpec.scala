// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.ensime.util.EnsimeSpec

class SexpCompactPrinterSpec extends EnsimeSpec {

  private val foo = SexpString("foo")
  private val foosym = SexpSymbol("foo")
  private val barsym = SexpSymbol("bar")
  private def assertPrinter(sexp: Sexp, expect: String): Unit = {
    SexpCompactPrinter(sexp) should ===(expect)
  }

  "CompactPrinter" should "handle nil or empty lists/data" in {
    assertPrinter(SexpNil, "nil")
    assertPrinter(SexpList(Nil), "nil")
  }

  it should "output lists of atoms" in {
    assertPrinter(SexpList(foo, SexpNumber(13), foosym), """("foo" 13 foo)""")
  }

  it should "output lists of lists" in {
    assertPrinter(SexpList(SexpList(foo), SexpList(foo)), """(("foo") ("foo"))""")
  }

  it should "output cons" in {
    assertPrinter(SexpCons(foosym, barsym), "(foo . bar)")
  }

  it should "output escaped characters" in {
    assertPrinter(SexpString("""C:\my\folder"""), """"C:\\my\\folder"""")
  }

}
