package org.jetbrains.plugins.scala
package lang
package parser
package parsing
package xml

import com.intellij.psi.xml.XmlTokenType
import org.jetbrains.plugins.scala.lang.lexer.ScalaXmlTokenTypes
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder

/**
  * @author Alexander Podkhalyuzin
  * Date: 18.04.2008
  */
/*
 * EmptyElemTag ::= '<' Name {Attribute} [S]'/>'
 */

object EmptyElemTag
  def parse(builder: ScalaPsiBuilder): Boolean =
    val tagMarker = builder.mark()
    builder.getTokenType match
      case ScalaXmlTokenTypes.XML_START_TAG_START =>
        builder.advanceLexer()
      case _ =>
        tagMarker.drop()
        return false
    builder.getTokenType match
      case ScalaXmlTokenTypes.XML_NAME =>
        builder.advanceLexer()
      case _ => builder error ErrMsg("xml.name.expected")
    while (Attribute.parse(builder)) {}
    builder.getTokenType match
      //looks like this code became obsolete long ago
      case XmlTokenType.XML_WHITE_SPACE => builder.advanceLexer()
      case _ =>
    builder.getTokenType match
      case ScalaXmlTokenTypes.XML_EMPTY_ELEMENT_END =>
        builder.advanceLexer()
        tagMarker.done(ScalaElementTypes.XML_EMPTY_TAG)
        true
      case _ =>
        tagMarker.rollbackTo()
        false
