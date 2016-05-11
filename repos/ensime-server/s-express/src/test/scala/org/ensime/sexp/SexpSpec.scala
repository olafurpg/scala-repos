// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.ensime.util.EnsimeSpec

class SexpSpec extends EnsimeSpec {

  val foostring = SexpString("foo")
  val barstring = SexpString("bar")
  val foosym = SexpSymbol("foo")
  val barsym = SexpSymbol("bar")
  val fookey = SexpSymbol(":foo")
  val barkey = SexpSymbol(":bar")

  "SexpList" should "create from varargs" in {
    SexpList(foosym, barsym) should ===(SexpList(List(foosym, barsym)))
  }

  it should "unroll as basic" in {
    SexpList(Nil) should ===(SexpNil)

    SexpList(foosym) should ===(SexpCons(foosym, SexpNil))

    SexpList(foosym, barsym) === (SexpCons(foosym, SexpCons(barsym, SexpNil)))
  }

  it should "match lists" in {
    SexpCons(foosym, SexpNil) match {
      case SexpList(els) if els == List(foosym) =>
      case _ => fail()
    }
    SexpCons(foosym, SexpCons(barsym, SexpNil)) match {
      case SexpList(els) if els == List(foosym, barsym) =>
      case _ => fail()
    }
    SexpNil match {
      case SexpList(_) => fail()
      case _ =>
    }
  }

  it should "support a list with 1000000 elements" in {
    val data = List.fill(1000000)(SexpChar('a'))
    SexpList(data) shouldBe a[Sexp]
  }

  "SexpData" should "create from varargs" in {
    SexpData(
      fookey -> barsym,
      barkey -> foosym
    ) should ===(SexpList(
        fookey, barsym,
        barkey, foosym
      ))
  }

  it should "unroll as basic" in {
    SexpData(
      fookey -> barsym,
      barkey -> foosym
    ) should ===(SexpCons(
        fookey, SexpCons(
        barsym, SexpCons(
        barkey, SexpCons(
        foosym, SexpNil
      )
      )
      )
      ))
  }

  it should "match SexpData" in {
    SexpCons(
      fookey, SexpCons(
      barsym, SexpCons(
      barkey, SexpCons(
      foosym, SexpNil
    )
    )
    )
    ) match {
      case SexpData(kvs) if kvs.size == 2 =>
      case _ => fail()
    }

    SexpNil match {
      case SexpData(_) => fail()
      case _ =>
    }
  }

  "SexpCons" should "unroll as fully basic" in {
    val a = SexpList(foosym)
    val b = SexpList(barsym)
    SexpCons(a, b) should ===(SexpCons(
      SexpCons(foosym, SexpNil),
      SexpCons(barsym, SexpNil)
    ))
  }
}
