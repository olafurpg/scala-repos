// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.ensime.util.EnsimeSpec

class SexpParserCheck
    extends EnsimeSpec with GeneratorDrivenPropertyChecks with ArbitrarySexp {

  import SexpParser.parse

  "SexpParser" should "round-trip Sexp <=> String" in {
    forAll { (sexp: Sexp) =>
      val compact = SexpCompactPrinter(sexp)
      //println(compact)
      val pretty = SexpPrettyPrinter(sexp)
      // it might be worthwhile creating a test-only printer that adds
      // superfluous whitespace/comments

      withClue(compact)(parse(compact) should ===(sexp))
      withClue(pretty)(parse(pretty) should ===(sexp))
    }
  }
}

trait ArbitrarySexp {

  import org.scalacheck.Arbitrary._
  import org.scalacheck.Gen._

  // avoid stackoverflows with http://stackoverflow.com/questions/19829293

  lazy val genSexpSymbol: Gen[SexpSymbol] =
    alphaStr.filter(_.nonEmpty).map(SexpSymbol)

  lazy val genSexpKey: Gen[SexpSymbol] = alphaStr.filter(_.nonEmpty).map { s =>
    SexpSymbol(":" + s)
  }

  // TODO: String/Char should be selected from a wider range
  // TODO: arbitrary[BigDecimal] but it freezes the tests
  // TODO: cons in SexpCons car, but it dramatically slows things
  lazy val genSexpAtom: Gen[SexpAtom] = oneOf(
      alphaNumChar.map(SexpChar),
      alphaStr.map(SexpString),
      genSexpSymbol,
      arbitrary[Double].map(SexpNumber(_)),
      //arbitrary[BigDecimal].map(SexpNumber(_)),
      oneOf(SexpNil, SexpPosInf, SexpNegInf, SexpNaN),
  )

  def genSexpCons(level: Int): Gen[SexpCons] =
    for {
      car <- genSexpAtom
      cdr <- genSexp(level + 1)
    } yield SexpCons(car, cdr)

  def genSexpList(level: Int): Gen[Sexp] =
    nonEmptyListOf(genSexp(level + 1)).map(SexpList(_))

  def genSexpData(level: Int): Gen[Sexp] =
    mapOfN(2, zip(genSexpKey, genSexp(level + 1))).map { kvs =>
      SexpData(kvs.toList)
    }

  // our parser is soooo slow for deep trees
  def genSexp(level: Int): Gen[Sexp] =
    if (level >= 4) genSexpAtom
    else
      lzy {
        oneOf(
            genSexpAtom,
            genSexpCons(level + 1),
            genSexpList(level + 1),
            genSexpData(level + 1),
        )
      }

  implicit def arbSexp: Arbitrary[Sexp] = Arbitrary(genSexp(0))
}
