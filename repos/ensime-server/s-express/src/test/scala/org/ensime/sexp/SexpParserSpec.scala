// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.ensime.util.EnsimeSpec

class SexpParserSpec extends EnsimeSpec {
  import SexpParser.parse

  val foo = SexpString("foo")
  val bar = SexpString("bar")
  val one = SexpNumber(1)
  val negtwo = SexpNumber(-2)
  val pi = SexpNumber("3.14")
  val fourexp = SexpNumber("4e+16")
  val foosym = SexpSymbol("foo")
  val barsym = SexpSymbol("bar")
  val fookey = SexpSymbol(":foo")
  val barkey = SexpSymbol(":bar")

  "EnrichedString" should "use the parser" in {
    "nil".parseSexp shouldBe SexpNil
  }

  "Sexp Parser" should "parse nil" in {
    parse("nil") shouldBe SexpNil
    parse("()") shouldBe SexpNil
    parse("( )") shouldBe SexpNil
    parse("(;blah\n)") shouldBe SexpNil
  }

  it should "parse lists of strings" in {
    parse("""("foo" "bar")""") shouldBe SexpList(foo, bar)
  }

  it should "parse escaped chars in strings" in {
    parse(""""z \\ \" \t \\t \\\t x\ x"""") shouldBe SexpString("z \\ \" \t \\t \\\t xx")

    parse(""""import foo\n\n\nexport bar\n"""") shouldBe SexpString("import foo\n\n\nexport bar\n")

    parse(""""C:\\my\\folder"""") shouldBe SexpString("""C:\my\folder""")
  }

  it should "parse unescaped chars in strings" in {
    parse("\"import foo\n\n\nexport bar\n\"") shouldBe SexpString("import foo\n\n\nexport bar\n")
  }

  it should "parse lists of chars" in {
    parse("""(?f ?b)""") shouldBe SexpList(SexpChar('f'), SexpChar('b'))
  }

  it should "parse lists of symbols" in {
    parse("(foo bar is?)") shouldBe SexpList(foosym, barsym, SexpSymbol("is?"))
  }

  it should "parse lists of numbers" in {
    parse("(1 -2 3.14 4e+16)") shouldBe SexpList(one, negtwo, pi, fourexp)
  }

  it should "parse NaN" in {
    parse("0.0e+NaN") shouldBe SexpNaN
    parse("-0.0e+NaN") shouldBe SexpNaN
  }

  it should "parse infinity" in {
    parse("1.0e+INF") shouldBe SexpPosInf
    parse("-1.0e+INF") shouldBe SexpNegInf
  }

  it should "parse lists within lists" in {
    parse("""((foo))""") shouldBe SexpList(SexpList(foosym))
    parse("""((foo) foo)""") shouldBe SexpList(SexpList(foosym), foosym)
  }

  it should "parse quoted expressions" in {
    parse("""'(:foo "foo" :bar "bar")""") shouldBe
      SexpCons(SexpSymbol("quote"), SexpList(fookey, foo, barkey, bar))

    parse("'foo") shouldBe SexpCons(SexpSymbol("quote"), foosym)
  }

  it should "parse cons" in {
    parse("(foo . bar)") shouldBe SexpCons(foosym, barsym)
  }

  it should "parse symbols with dots in their name" in {
    parse("foo.bar") shouldBe SexpSymbol("foo.bar")
    parse(":foo.bar") shouldBe SexpSymbol(":foo.bar")
  }

  it should "parse symbols starting with nil in their name" in {
    parse("nilsamisanidiot") shouldBe SexpSymbol("nilsamisanidiot")
  }
}
