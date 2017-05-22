package org.jetbrains.plugins.scala
package lang
package completion
package filters.other

import com.intellij.psi.filters.ElementFilter
import com.intellij.psi.{PsiElement, _}
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.lang.completion.ScalaCompletionUtil._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScInfixTypeElement

/** 
  * @author Alexander Podkhalyuzin
  * Date: 28.05.2008
  */
class ForSomeFilter extends ElementFilter
  def isAcceptable(element: Object, context: PsiElement): Boolean =
    if (context.isInstanceOf[PsiComment]) return false
    val leaf = getLeafByOffset(context.getTextRange.getStartOffset, context)
    if (leaf != null)
      val parent = leaf.getParent
      if (parent == null) return false
      parent.getParent match
        case _: ScInfixTypeElement => return true
        case _ => return false
    false

  def isClassAcceptable(hintClass: java.lang.Class[_]): Boolean =
    true

  @NonNls
  override def toString: String =
    "'forSome' keyword filter"
