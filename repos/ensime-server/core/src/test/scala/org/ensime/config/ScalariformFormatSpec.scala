// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.util.EnsimeSpec

import scalariform.formatter.preferences._

class ScalariformFormatSpec extends EnsimeSpec {

  object ScalariformProtocol extends DefaultSexpProtocol with ScalariformFormat
  import ScalariformProtocol._

  val prefs = FormattingPreferences().
    setPreference(DoubleIndentClassDeclaration, true).
    setPreference(IndentSpaces, 13)

  "ScalariformFormat" should "parse some example config" in {
    val text = """(:doubleIndentClassDeclaration t
                     :indentSpaces 13)"""
    val recover = text.parseSexp.convertTo[FormattingPreferences]
    recover.preferencesMap shouldBe prefs.preferencesMap
  }

  it should "create valid output" in {
    prefs.toSexp should ===(SexpList(
      SexpSymbol(":doubleIndentClassDeclaration"), SexpSymbol("t"),
      SexpSymbol(":indentSpaces"), SexpNumber(13)
    ))
  }
}
