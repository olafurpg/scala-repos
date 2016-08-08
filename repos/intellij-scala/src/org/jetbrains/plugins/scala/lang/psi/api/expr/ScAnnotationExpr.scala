package org.jetbrains.plugins.scala
package lang
package psi
package api
package expr

import com.intellij.psi.{PsiAnnotationMemberValue, PsiElement}
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructor
import org.jetbrains.plugins.scala.lang.psi.impl.expr.ScNameValuePairImpl

/**
  * @author Alexander Podkhalyuzin
  * Date: 07.03.2008
  */
trait ScAnnotationExpr extends ScalaPsiElement {
  def constr = findChildByClassScala(classOf[ScConstructor])
  def getAttributes: Seq[ScNameValuePair] =
    findArgExprs
      .map(_.findChildrenByType(ScalaElementTypes.ASSIGN_STMT))
      .getOrElse(Seq.empty)
      .map {
        case stmt: ScAssignStmt => new ScNameValueAssignment(stmt)
      }

  def getAnnotationParameters = findArgExprs.map(_.exprs).getOrElse(Seq.empty)

  private def findArgExprs: Option[ScArgumentExprList] = {
    val constr = findChildByClassScala(classOf[ScConstructor])
    if (constr == null) return None

    val args = constr.findFirstChildByType(ScalaElementTypes.ARG_EXPRS)
    args match {
      case scArgExpr: ScArgumentExprList => Some(scArgExpr)
      case _ => None
    }
  }

  private class ScNameValueAssignment(assign: ScAssignStmt)
      extends ScNameValuePairImpl(assign.getNode) {
    override def nameId: PsiElement = assign.getLExpression

    override def getValue: PsiAnnotationMemberValue =
      (assign.getRExpression map {
        case annotationMember: PsiAnnotationMemberValue => annotationMember
        case _ => null
      }).orNull
  }
}
