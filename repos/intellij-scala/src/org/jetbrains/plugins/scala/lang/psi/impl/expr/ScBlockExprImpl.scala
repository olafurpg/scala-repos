package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import java.util

import com.intellij.psi.impl.source.tree.LazyParseablePsiElement
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/**
  * @author Alexander Podkhalyuzin
  * Date: 06.03.2008
  */
class ScBlockExprImpl(text: CharSequence)
    extends LazyParseablePsiElement(ScalaElementTypes.BLOCK_EXPR, text)
    with ScBlockExpr

  //todo: bad architecture to have it duplicated here, as ScBlockExprImpl is not instance of ScalaPsiElementImpl
  override def getContext: PsiElement =
    context match
      case null => super.getContext
      case _ => context

  override def toString: String = "BlockExpression"

  override def hasCaseClauses: Boolean = caseClauses.isDefined

  protected def findChildrenByClassScala[T >: Null <: ScalaPsiElement](
      aClass: Class[T]): Array[T] =
    val result: util.List[T] = new util.ArrayList[T]
    var cur: PsiElement = getFirstChild
    while (cur != null)
      if (aClass.isInstance(cur)) result.add(cur.asInstanceOf[T])
      cur = cur.getNextSibling
    result.toArray[T](java.lang.reflect.Array
          .newInstance(aClass, result.size)
          .asInstanceOf[Array[T]])

  protected def findChildByClassScala[T >: Null <: ScalaPsiElement](
      aClass: Class[T]): T =
    var cur: PsiElement = getFirstChild
    while (cur != null)
      if (aClass.isInstance(cur)) return cur.asInstanceOf[T]
      cur = cur.getNextSibling
    null

  override def accept(visitor: ScalaElementVisitor) =
    visitor.visitBlockExpression(this)

  override def accept(visitor: PsiElementVisitor)
    visitor match
      case s: ScalaElementVisitor => accept(s)
      case _ => super.accept(visitor)
