package org.jetbrains.plugins.scala
package annotator
package gutter

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile, PsiMethod}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.{
  ScAssignStmt, ScSelfInvocation
}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{
  ScObject, ScTypeDefinition
}
import org.jetbrains.plugins.scala.lang.resolve.ResolvableReferenceElement

/**
  * User: Alexander Podkhalyuzin
  * Date: 22.11.2008
  */
class ScalaGoToDeclarationHandler extends GotoDeclarationHandler {

  def getActionText(context: DataContext): String = null

  def getGotoDeclarationTargets(_sourceElement: PsiElement,
                                offset: Int,
                                editor: Editor): Array[PsiElement] = {
    if (_sourceElement == null) return null
    val containingFile: PsiFile = _sourceElement.getContainingFile
    if (containingFile == null) return null
    val sourceElement = containingFile.findElementAt(offset)
    if (sourceElement == null) return null
    if (sourceElement.getLanguage != ScalaFileType.SCALA_LANGUAGE) return null

    if (sourceElement.getNode.getElementType == ScalaTokenTypes.tASSIGN) {
      return sourceElement.getParent match {
        case assign: ScAssignStmt =>
          val elem = assign.assignNavigationElement
          Option(elem).toArray
        case _ => null
      }
    }

    if (sourceElement.getNode.getElementType == ScalaTokenTypes.kTHIS) {
      sourceElement.getParent match {
        case self: ScSelfInvocation =>
          self.bind match {
            case Some(elem) => return Array(elem)
            case None => return null
          }
        case _ => return null
      }
    }

    if (sourceElement.getNode.getElementType == ScalaTokenTypes.tIDENTIFIER) {
      val file = sourceElement.getContainingFile
      val ref = file.findReferenceAt(sourceElement.getTextRange.getStartOffset)
      if (ref == null) return null
      val targets = ref match {
        case resRef: ResolvableReferenceElement =>
          resRef.bind() match {
            case Some(x) =>
              /**
                * Extra targets:
                *
                * actualElement              type alias used to access a constructor.
                *                            See also [[org.jetbrains.plugins.scala.findUsages.TypeAliasUsagesSearcher]]
                * innerResolveResult#element apply method
                */
              val all =
                Seq(x.getActualElement, x.element) ++ x.innerResolveResult.map(
                    _.getElement)
              x.element match {
                case f: ScFunction if f.isSynthetic =>
                  Seq(x.getActualElement).flatMap(goToTargets)
                case c: PsiMethod if c.isConstructor =>
                  val clazz = c.containingClass
                  if (clazz == x.getActualElement)
                    Seq(x.element).flatMap(goToTargets)
                  else all.distinct flatMap goToTargets
                case _ =>
                  all.distinct flatMap goToTargets
              }
            case None => return null
          }
        case r =>
          Set(r.resolve()) flatMap goToTargets
      }
      return targets.toArray
    }
    null
  }

  private def goToTargets(element: PsiElement): Seq[PsiElement] = {
    element match {
      case null => Seq.empty
      case fun: ScFunction =>
        Seq(fun.getSyntheticNavigationElement.getOrElse(element))
      case td: ScTypeDefinition if td.isSynthetic =>
        td.syntheticContainingClass match {
          case Some(containingClass) => Seq(containingClass)
          case _ => Seq(element)
        }
      case o: ScObject if o.isSyntheticObject =>
        Seq(ScalaPsiUtil.getCompanionModule(o).getOrElse(element))
      case param: ScParameter =>
        ScalaPsiUtil
          .parameterForSyntheticParameter(param)
          .map(Seq[PsiElement](_))
          .getOrElse(Seq[PsiElement](element))
      case _ => Seq(element)
    }
  }
}
