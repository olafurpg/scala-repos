// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

import org.parboiled2._

import scala.util.{ Failure, Success }

/**
 * Parse Emacs Lisp into an `Sexp`. Other lisp variants may
 * require tweaking, e.g. Scheme's nil, infinity, NaN, etc.
 */
object SexpParser {

  def parse(desc: String): Sexp = {
    val parser = new SexpParser(desc)
    parser.SexpP.run() match {
      case Success(d) =>
        d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception("Failed to parse sexp: " + msg)
      case Failure(other) =>
        throw new Exception("Failed to parse sexp: ", other)
    }
  }

  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-for-Strings.html
  // Not supported: https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
  private[sexp] val specialChars = Map[String, String](
    "\"" -> "\"",
    "a" -> "\u0007",
    "b" -> "\b",
    "t" -> "\t",
    "n" -> "\n",
    "v" -> "\u000b",
    "f" -> "\f",
    "r" -> "\r",
    "e" -> "\u001b",
    "s" -> " ",
    "d" -> "\u007f",
    "\\" -> "\\"
  )

  val SexpQuote = SexpSymbol("quote")
  val SymbolsPredicate = CharPredicate("+-*/_~!@$%^&=:<>{}")
  val NormalCharPredicate = CharPredicate.Printable -- "\"\\"
  val WhiteSpacePredicate = CharPredicate(" \n\r\t\f")
  val NotNewLinePredicate = CharPredicate.Printable -- '\n'
  val SymbolStartCharPredicate = CharPredicate.AlphaNum ++ SymbolsPredicate
  val SymbolBodyCharPredicate = SymbolStartCharPredicate ++ "."
  val PlusMinusPredicate = CharPredicate("+-")
  val ExpPredicate = CharPredicate("eE")

  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
  val NCCharPredicate = CharPredicate.All -- "\"\\"

}

/**
 * Parse Emacs Lisp into an `Sexp`. Other lisp variants may
 * require tweaking, e.g. Scheme's nil, infinity, NaN, etc.
 */
class SexpParser(val input: ParserInput) extends Parser with StringBuilding {

  import SexpParser._
  private def SexpP: Rule1[Sexp] = rule {
    SexpAtomP | SexpListP | SexpEmptyList | SexpConsP | SexpQuotedP
  }

  private def SexpConsP: Rule1[SexpCons] = rule {
    LeftBrace ~ SexpP ~ Whitespace ~ '.' ~ Whitespace ~ SexpP ~ RightBrace ~> {
      (x: Sexp, y: Sexp) => SexpCons(x, y)
    }
  }

  private def SexpListP: Rule1[Sexp] = rule {
    LeftBrace ~ SexpP ~ zeroOrMore(Whitespace ~ SexpP) ~ RightBrace ~> {
      (head: Sexp, tail: Seq[Sexp]) => { SexpList(head :: tail.toList) }
    }
  }

  private def SexpAtomP: Rule1[SexpAtom] = rule {
    SexpCharP | SexpStringP | SexpNaNP | SexpNumberP | SexpSymbolP
  }

  private def SexpCharP: Rule1[SexpChar] = rule {
    '?' ~ NormalChar ~> { SexpChar }
  }

  def SexpStringP = rule { '"' ~ clearSB() ~ CharactersSB ~ '"' ~ push(SexpString(sb.toString)) }

  def CharactersSB = rule { zeroOrMore(NormalCharSB | '\\' ~ EscapedCharSB) }

  def NormalCharSB = rule { NCCharPredicate ~ appendSB() }

  def EscapedCharSB = rule(
    QuoteSlashBackSlash ~ appendSB()
      | '\"' ~ appendSB('\"')
      | 'b' ~ appendSB('\b')
      | 's' ~ appendSB(' ')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | ' ' ~ appendSB("") // special emacs magic for comments \<space< and \<newline> are removed
      | '\n' ~ appendSB("")
      | 'a' ~ appendSB('\u0007') // bell
      | 'v' ~ appendSB('\u000b') // vertical tab
      | 'e' ~ appendSB('\u001b') // escape
      | 'd' ~ appendSB('\u007f') // DEL
  )

  def SexpNumberP = rule {
    capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> { s: String => SexpNumber(BigDecimal(s)) }
  }

  import CharPredicate.{ Digit, Digit19 }

  def Integer = rule {
    optional('-') ~ (Digit19 ~ Digits | Digit)
  }

  def Digits = rule {
    oneOrMore(Digit)
  }

  def Frac = rule {
    '.' ~ Digits
  }

  def Exp = rule {
    ExpPredicate ~ optional(PlusMinusPredicate) ~ Digits
  }

  private def SexpNaNP: Rule1[SexpAtom] = rule {
    "-1.0e+INF" ~ push(SexpNegInf) |
      "1.0e+INF" ~ push(SexpPosInf) |
      optional('-') ~ "0.0e+NaN" ~ push(SexpNaN)
  }

  private def SexpQuotedP: Rule1[Sexp] = rule {
    '\'' ~ SexpP ~> { v: Sexp => SexpCons(SexpQuote, v) }
  }

  private def SexpSymbolP: Rule1[SexpAtom] = rule {
    // ? allowed at the end of symbol names
    capture(oneOrMore(SymbolStartCharPredicate) ~ zeroOrMore(SymbolBodyCharPredicate) ~ optional('?')) ~> { sym: String =>
      if (sym == "nil") SexpNil
      else SexpSymbol(sym)
    }
  }

  private def SexpEmptyList: Rule1[SexpNil.type] = rule {
    LeftBrace ~ RightBrace ~ push(SexpNil)
  }

  private def NormalChar: Rule1[Char] = rule {
    NormalCharPredicate ~ push(lastChar)
  }

  private def Whitespace: Rule0 = rule {
    zeroOrMore(Comment | WhiteSpacePredicate)
  }

  private def Comment: Rule0 = rule {
    ';' ~ zeroOrMore(NotNewLinePredicate) ~ ("\n" | EOI)
  }

  private def LeftBrace: Rule0 = rule {
    Whitespace ~ '(' ~ Whitespace
  }

  private def RightBrace: Rule0 = rule {
    Whitespace ~ ')' ~ Whitespace
  }

}
