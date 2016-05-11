// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.parboiled2.Parser
import org.parboiled2.Rule1
import org.parboiled2._

import scala.annotation.switch
import scala.util.{ Failure, Success }

object DescriptorParser {
  def parse(desc: String): Descriptor = {
    val parser = new DescriptorParser(desc)
    parser.Desc.run() match {
      case Success(d) => d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception(s"Failed to parse descriptor: $msg")
      case Failure(other) =>
        throw new Exception("Failed to parse descriptor: ", other)
    }
  }

  def parseType(desc: String): DescriptorType = {
    val parser = new DescriptorParser(desc)
    parser.Type.run() match {
      case Success(d) => d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception(s"Failed to parse descriptor type: $msg")
      case Failure(other) =>
        throw new Exception("Failed to parse descriptor type: ", other)
    }
  }
  val PackageNamePredicate = CharPredicate.All -- ";/ "
  val ClassNameCharPredicate = CharPredicate.All -- ";/ "
}

class DescriptorParser(val input: ParserInput) extends Parser {
  import ClassName._

  def Desc: Rule1[Descriptor] = rule {
    '(' ~ zeroOrMore(Type) ~ ')' ~ Type ~ EOI ~> {
      (paramSeq: Seq[DescriptorType], retType: DescriptorType) => Descriptor(paramSeq.toList, retType)
    }
  }

  def Type: Rule1[DescriptorType] = rule {
    // based on the example in the JSON Parser from Parboiled2 doing a one character lookahead here
    // all descriptor types can be inferred from the first character
    run {
      (cursorChar: @switch) match {
        case 'L' => Class
        case 'Z' => Boolean
        case 'B' => Byte
        case 'C' => Char
        case 'S' => Short
        case 'I' => Int
        case 'J' => Long
        case 'F' => Float
        case 'D' => Double
        case 'V' => Void
        case '[' => Array
        case _ => MISMATCH
      }
    }
  }

  private def Class: Rule1[DescriptorType] = rule {
    'L' ~ Package ~ Name ~ ';' ~> ClassName.apply _
  }

  private def Name: Rule1[String] = rule {
    capture(oneOrMore(DescriptorParser.ClassNameCharPredicate))
  }

  private def Package: Rule1[PackageName] = rule {
    zeroOrMore(capture(oneOrMore(DescriptorParser.PackageNamePredicate)) ~ '/') ~> { seq: Seq[String] => PackageName(seq.toList) }
  }

  private def Array: Rule1[DescriptorType] = rule {
    '[' ~ Type ~> { c => ArrayDescriptor(c) }
  }

  private def Boolean: Rule1[ClassName] = rule { 'Z' ~ push(PrimitiveBoolean) }
  private def Byte: Rule1[ClassName] = rule { 'B' ~ push(PrimitiveByte) }
  private def Char: Rule1[ClassName] = rule { 'C' ~ push(PrimitiveChar) }
  private def Short: Rule1[ClassName] = rule { 'S' ~ push(PrimitiveShort) }
  private def Int: Rule1[ClassName] = rule { 'I' ~ push(PrimitiveInt) }
  private def Long: Rule1[ClassName] = rule { 'J' ~ push(PrimitiveLong) }
  private def Float: Rule1[ClassName] = rule { 'F' ~ push(PrimitiveFloat) }
  private def Double: Rule1[ClassName] = rule { 'D' ~ push(PrimitiveDouble) }
  private def Void: Rule1[ClassName] = rule { 'V' ~ push(PrimitiveVoid) }
}
