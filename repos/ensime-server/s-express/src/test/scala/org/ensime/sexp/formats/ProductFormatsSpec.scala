// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import org.ensime.sexp._

class ProductFormatsSpec extends FormatSpec
    with BasicFormats with StandardFormats with ProductFormats {

  case class Foo(i: Int, s: String)
  case class Bar(foo: Foo)
  case class Baz()
  case class Wibble(thing: String, thong: Int, bling: Option[String])

  val foo = Foo(13, "foo")
  val fooexpect = SexpData(
    SexpSymbol(":i") -> SexpNumber(13),
    SexpSymbol(":s") -> SexpString("foo")
  )

  "ProductFormats case classes" should "support primitive types" in {
    // will create the marshaller every time assertFormat is called
    assertFormat(foo, fooexpect)
    assertFormat(foo, fooexpect)
    assertFormat(foo, fooexpect)
  }

  it should "support 'fast' case classes" in {
    // can't really test - its a side effect optimisation
    implicit val FastFooFormat = SexpFormat[Foo]
    assertFormat(foo, fooexpect)
    assertFormat(foo, fooexpect)
    assertFormat(foo, fooexpect)
  }

  it should "support nested case classes" in {
    val bar = Bar(foo)
    val expect = SexpData(
      SexpSymbol(":foo") -> fooexpect
    )

    // (this is actually a really big deal, thank you shapeless!)
    assertFormat(bar, expect)
  }

  it should "support zero content case classes" in {
    assertFormat(Baz(), SexpNil)
  }

  it should "support missing fields as SexpNil / None" in {
    val wibble = Wibble("wibble", 13, Some("fork"))

    assertFormat(wibble, SexpData(
      SexpSymbol(":thing") -> SexpString("wibble"),
      SexpSymbol(":thong") -> SexpNumber(13),
      SexpSymbol(":bling") -> SexpList(SexpString("fork"))
    ))

    val wobble = Wibble("wibble", 13, None)

    // write out None as SexpNil
    assertFormat(wobble, SexpData(
      SexpSymbol(":thing") -> SexpString("wibble"),
      SexpSymbol(":thong") -> SexpNumber(13),
      SexpSymbol(":bling") -> SexpNil
    ))

    // but tolerate missing entries
    SexpData(
      SexpSymbol(":thing") -> SexpString("wibble"),
      SexpSymbol(":thong") -> SexpNumber(13)
    ).convertTo[Wibble] should ===(wobble)
  }

  val bar = (13, "bar")
  val barexpect = SexpList(SexpNumber(13), SexpString("bar"))

  "ProductFormat tuples" should "support primitive types" in {
    assertFormat(bar, barexpect)
  }

  it should "support 'fast' tuples" in {
    // can't really test - its a side effect optimisation
    implicit val FastBarormat = SexpFormat[(Int, String)]
    assertFormat(bar, barexpect)
    assertFormat(bar, barexpect)
    assertFormat(bar, barexpect)
  }
}

class CustomisedProductFormatsSpec extends FormatSpec
    with BasicFormats with StandardFormats with ProductFormats
    with CamelCaseToDashes {

  trait SkippingEnabled extends ProductFormats {
    override val skipNilValues = true
  }

  case class Foo(AThingyMaBob: Int, HTML: String)
  case class Bar(num: Int, str: Option[String])

  "ProductFormats with overloaded toWireName" should "support custom field names" in {
    assertFormat(Foo(13, "foo"), SexpData(
      SexpSymbol(":a-thingy-ma-bob") -> SexpNumber(13),
      SexpSymbol(":h-t-m-l") -> SexpString("foo")
    ))
  }

  "ProductFormats" should "not skip writing out nil values by default" in {
    val wobble = Bar(13, None)
    assertFormat(wobble, SexpData(
      SexpSymbol(":num") -> SexpNumber(13),
      SexpSymbol(":str") -> SexpNil
    ))
  }

  "ProductFormats with overloaded skipNilValues" should "support writing out only non-nil values" in new SkippingEnabled {
    val wobble = Bar(13, None)
    assertFormat(wobble, SexpData(
      SexpSymbol(":num") -> SexpNumber(13)
    ))
  }
}

