// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.ensime.util.EnsimeSpec

class SexpPrettyPrinterSpec extends EnsimeSpec {

  private val foo = SexpString("foo")
  private val foosym = SexpSymbol("foo")
  private val barsym = SexpSymbol("bar")
  private val fookey = SexpSymbol(":foo")
  private val barkey = SexpSymbol(":bar")
  private def assertPrinter(sexp: Sexp, expect: String): Unit = {
    //    println("GOT\n" + SexpPrettyPrinter(sexp))
    //    println("EXPECT\n" + expect)
    SexpPrettyPrinter(sexp) should ===(expect.replace("\r", ""))
  }

  "CompactPrinter" should "handle nil or empty lists/data" in {
    assertPrinter(SexpNil, "nil")
    assertPrinter(SexpList(Nil), "nil")
  }

  it should "output lists of atoms" in {
    assertPrinter(
      SexpList(foo, SexpNumber(13), foosym),
      """("foo"
          |  13
          |  foo)""".stripMargin
    )
  }

  it should "output lists of lists" in {
    assertPrinter(
      SexpList(SexpList(foo), SexpList(foo)),
      """(("foo")
          |  ("foo"))""".stripMargin
    )
  }

  it should "output data" in {
    assertPrinter(
      SexpData(fookey -> foosym, barkey -> foosym),
      """(
  :foo foo
  :bar foo
)"""
    )

    val datum = SexpData(fookey -> foo, barkey -> foo)
    assertPrinter(SexpData(
      fookey -> datum,
      barkey -> datum
    ), """(
  :foo (
    :foo "foo"
    :bar "foo"
  )
  :bar (
    :foo "foo"
    :bar "foo"
  )
)""")
  }

  it should "output cons" in {
    assertPrinter(SexpCons(foosym, barsym), "(foo .\n  bar)")
  }

}

