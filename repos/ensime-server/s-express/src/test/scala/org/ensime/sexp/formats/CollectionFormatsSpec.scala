// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import collection.{ immutable => im }

import org.ensime.sexp._

// http://docs.scala-lang.org/overviews/collections/overview.html
class CollectionFormatsSpec extends FormatSpec
    with ProductFormats with CollectionFormats with BasicFormats {

  val foo = SexpString("foo")
  val foos: List[String] = List("foo", "foo")
  val expect = SexpList(foo, foo)

  "CollectionFormats traits" should "support Traversable" in {
    assertFormat(collection.Traversable[String](), SexpNil)
    assertFormat(collection.Traversable(foos: _*), expect)
  }

  it should "support Iterable" in {
    assertFormat(collection.Iterable[String](), SexpNil)
    assertFormat(collection.Iterable(foos: _*), expect)
  }

  it should "support Seq" in {
    assertFormat(collection.Seq[String](), SexpNil)
    assertFormat(collection.Seq(foos: _*), expect)
  }

  it should "support IndexedSeq" in {
    assertFormat(collection.IndexedSeq[String](), SexpNil)
    assertFormat(collection.IndexedSeq(foos: _*), expect)
  }

  it should "support LinearSeq" in {
    assertFormat(collection.LinearSeq[String](), SexpNil)
    assertFormat(collection.LinearSeq(foos: _*), expect)
  }

  it should "support Set" in {
    assertFormat(collection.Set[String](), SexpNil)
    assertFormat(collection.Set(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support SortedSet" in {
    assertFormat(collection.SortedSet[String](), SexpNil)
    assertFormat(collection.SortedSet(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support BitSet" in {
    assertFormat(collection.BitSet(), SexpNil)
    assertFormat(collection.BitSet(0, 1), SexpString("16#3"))
    assertFormat(collection.BitSet(64), SexpString("16#10000000000000000"))
    assertFormat(collection.BitSet(0, 64), SexpString("16#10000000000000001"))
    assertFormat(collection.BitSet(1, 64), SexpString("16#10000000000000002"))
  }

  it should "support Map" in {
    assertFormat(collection.Map[String, String](), SexpNil)
    assertFormat(collection.Map("foo" -> "foo"), SexpList(SexpList(foo, foo)))
  }

  it should "support SortedMap" in {
    assertFormat(collection.SortedMap[String, String](), SexpNil)
    assertFormat(collection.SortedMap("foo" -> "foo"), SexpList(SexpList(foo, foo)))
  }

  "CollectionFormats immutable variants of the traits" should "support Traversable" in {
    assertFormat(im.Traversable[String](), SexpNil)
    assertFormat(im.Traversable(foos: _*), expect)
  }

  it should "support Iterable" in {
    assertFormat(im.Iterable[String](), SexpNil)
    assertFormat(im.Iterable(foos: _*), expect)
  }

  it should "support Seq" in {
    assertFormat(im.Seq[String](), SexpNil)
    assertFormat(im.Seq(foos: _*), expect)
  }

  it should "support IndexedSeq" in {
    assertFormat(im.IndexedSeq[String](), SexpNil)
    assertFormat(im.IndexedSeq(foos: _*), expect)
  }

  it should "support LinearSeq" in {
    assertFormat(im.LinearSeq[String](), SexpNil)
    assertFormat(im.LinearSeq(foos: _*), expect)
  }

  it should "support Set" in {
    assertFormat(im.Set[String](), SexpNil)
    assertFormat(im.Set(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support SortedSet" in {
    assertFormat(im.SortedSet[String](), SexpNil)
    assertFormat(im.SortedSet(foos: _*), SexpList(foo)) // dupes removed
  }

  it should "support BitSet" in {
    assertFormat(im.BitSet(), SexpNil)
    assertFormat(im.BitSet(0, 1), SexpString("16#3"))
    assertFormat(collection.BitSet(64), SexpString("16#10000000000000000"))
    assertFormat(collection.BitSet(0, 64), SexpString("16#10000000000000001"))
    assertFormat(collection.BitSet(1, 64), SexpString("16#10000000000000002"))
  }

  it should "support Map" in {
    assertFormat(im.Map[String, String](), SexpNil)
    assertFormat(im.Map("foo" -> "foo"), SexpList(SexpList(foo, foo)))
  }

  it should "support SortedMap" in {
    assertFormat(im.SortedMap[String, String](), SexpNil)
    assertFormat(im.SortedMap("foo" -> "foo"), SexpList(SexpList(foo, foo)))
  }

  "CollectionFormats immutable specific implementations" should "support im.List" in {
    assertFormat(im.List[String](), SexpNil)
    assertFormat(im.List(foos: _*), expect)
  }

  it should "support im.Vector" in {
    assertFormat(im.Vector[String](), SexpNil)
    assertFormat(im.Vector(foos: _*), expect)
  }

  it should "support im.Range" in {
    assertFormat(
      im.Range(-100, 100),
      SexpList(
        SexpSymbol(":start"), SexpNumber(-100),
        SexpSymbol(":end"), SexpNumber(100),
        SexpSymbol(":step"), SexpNumber(1)
      )
    )

    assertFormat(
      im.Range(-100, 100, 2),
      SexpList(
        SexpSymbol(":start"), SexpNumber(-100),
        SexpSymbol(":end"), SexpNumber(100),
        SexpSymbol(":step"), SexpNumber(2)
      )
    )
  }

  it should "support im.NumericRange" in {
    implicit val DoubleIntegral = Numeric.DoubleAsIfIntegral

    assertFormat(
      -100.0 to 100.0 by 1.5,
      SexpData(
        SexpSymbol(":start") -> SexpNumber(-100),
        SexpSymbol(":end") -> SexpNumber(100),
        SexpSymbol(":step") -> SexpNumber(1.5),
        SexpSymbol(":inclusive") -> SexpSymbol("t")
      )
    )

    assertFormat(
      -100.0 until 100.0 by 1.5,
      SexpData(
        SexpSymbol(":start") -> SexpNumber(-100),
        SexpSymbol(":end") -> SexpNumber(100),
        SexpSymbol(":step") -> SexpNumber(1.5),
        SexpSymbol(":inclusive") -> SexpNil
      )
    )
  }

}
