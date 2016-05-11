// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import org.ensime.sexp._

// these are a sort of aggregated test, investigating behaviours of
// interactions between the different formats.
class DefaultSexpProtocolSpec extends FormatSpec {

  "DefaultSexpProtocol" should "support String as SexpString, not via IsTraversableLike" in {
    import DefaultSexpProtocol._
    assertFormat("hello", SexpString("hello"))
  }

  it should "round-trip Option[List[T]]" in {
    import DefaultSexpProtocol._

    val none: Option[List[String]] = None
    val empty: Option[List[String]] = Some(Nil)
    val list: Option[List[String]] = Some(List("boo"))

    assertFormat(none, SexpNil)
    assertFormat(empty, SexpList(SexpNil))
    assertFormat(list, SexpList(SexpList(SexpString("boo"))))
    SexpNil.convertTo[Option[List[String]]] should ===(None)
    SexpNil.convertTo[List[Option[String]]] should ===(Nil)
  }

  object AlternativeProtocol extends DefaultSexpProtocol with OptionAltFormat

  "DefaultSexpProtocol with OptionAltFormat" should "predictably fail to round-trip Option[List[T]]" in {
    import AlternativeProtocol._

    val none: Option[List[String]] = None
    val empty: Option[List[String]] = Some(Nil)
    val list: Option[List[String]] = Some(List("boo"))

    none.toSexp should ===(SexpNil)
    empty.toSexp should ===(SexpNil)
    list.toSexp should ===(SexpList(SexpString("boo")))

    SexpNil.convertTo[Option[List[String]]] should ===(None)

    // and Lists of Options give empty lists, muahahaha
    SexpNil.convertTo[List[Option[String]]] should ===(Nil)
  }

}
