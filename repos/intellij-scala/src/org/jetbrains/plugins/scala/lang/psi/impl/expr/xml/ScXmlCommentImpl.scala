package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr
package xml

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.lang.psi.api.expr.xml._

/**
  * @author Alexander Podkhalyuzin
  * Date: 21.04.2008
  */
class ScXmlCommentImpl(node: ASTNode)
    extends ScalaPsiElementImpl(node) with ScXmlComment
  override def toString: String = "XmlComment"
