package org.jetbrains.plugins.scala
package codeInsight.intention.expression

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.util.{IntentionAvailabilityChecker, IntentionUtils}

/**
  * @author Ksenia.Sautina
  * @since 4/9/12
  */
object ConvertToInfixExpressionIntention {
  val familyName = "Convert to infix expression"
}

class ConvertToInfixExpressionIntention extends PsiElementBaseIntentionAction {
  def getFamilyName = ConvertToInfixExpressionIntention.familyName

  override def getText: String = getFamilyName

  def isAvailable(project: Project,
                  editor: Editor,
                  element: PsiElement): Boolean = {
    if (!IntentionAvailabilityChecker.checkIntention(this, element))
      return false
    val methodCallExpr: ScMethodCall =
      PsiTreeUtil.getParentOfType(element, classOf[ScMethodCall], false)
    if (methodCallExpr == null) return false
    val referenceExpr = methodCallExpr.getInvokedExpr match {
      case ref: ScReferenceExpression => ref
      case call: ScGenericCall =>
        Option(call.referencedExpr) match {
          //if the expression has type args
          case Some(ref: ScReferenceExpression) => ref
          case _ => return false
        }
      case _ => return false
    }
    val range: TextRange = referenceExpr.nameId.getTextRange
    val offset = editor.getCaretModel.getOffset
    if (!(range.getStartOffset <= offset && offset <= range.getEndOffset))
      return false
    if (referenceExpr.isQualified) return true
    false
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement) {
    val methodCallExpr: ScMethodCall =
      PsiTreeUtil.getParentOfType(element, classOf[ScMethodCall], false)
    if (methodCallExpr == null || !methodCallExpr.isValid) return

    val referenceExpr = methodCallExpr.getInvokedExpr match {
      case ref: ScReferenceExpression => ref
      case call: ScGenericCall =>
        Option(call.referencedExpr) match {
          //if the expression has type args
          case Some(ref: ScReferenceExpression) => ref
          case _ => return
        }
      case _ => return
    }
    val start = methodCallExpr.getTextRange.getStartOffset
    val diff =
      editor.getCaretModel.getOffset -
        referenceExpr.nameId.getTextRange.getStartOffset

    var putArgsFirst = false
    val argsBuilder = new StringBuilder
    val invokedExprBuilder = new StringBuilder

    val qual = referenceExpr.qualifier.get
    val operText = methodCallExpr.getInvokedExpr match {
      case call: ScGenericCall =>
        call.typeArgs match {
          case Some(typeArgs) =>
            referenceExpr.nameId.getText ++ typeArgs.getText
          case _ => referenceExpr.nameId.getText
        }
      case _ => referenceExpr.nameId.getText
    }
    val invokedExprText = methodCallExpr.getInvokedExpr.getText
    val methodCallArgs = methodCallExpr.args

    if (invokedExprText.last == ':') {
      putArgsFirst = true
      invokedExprBuilder.append(operText).append(" ").append(qual.getText)
    } else {
      invokedExprBuilder.append(qual.getText).append(" ").append(operText)
    }

    argsBuilder.append(methodCallArgs.getText)

    IntentionUtils.analyzeMethodCallArgs(methodCallArgs, argsBuilder)

    var forA = qual.getText
    if (forA.startsWith("(") && forA.endsWith(")")) {
      forA = qual.getText.drop(1).dropRight(1)
    }

    var forB = argsBuilder.toString()
    if (forB.startsWith("(") && forB.endsWith(")")) {
      forB = argsBuilder.toString().drop(1).dropRight(1)
    }

    val exprA: ScExpression =
      ScalaPsiElementFactory.createExpressionFromText(forA, element.getManager)
    val exprB: ScExpression =
      ScalaPsiElementFactory.createExpressionFromText(forB, element.getManager)

    val expr = putArgsFirst match {
      case true => argsBuilder.append(" ").append(invokedExprBuilder)
      case false => invokedExprBuilder.append(" ").append(argsBuilder)
    }

    val text = expr.toString()
    ScalaPsiElementFactory
      .createExpressionFromText(text, element.getManager) match {
      case infixExpr: ScInfixExpr =>
        infixExpr
          .asInstanceOf[ScInfixExpr]
          .getBaseExpr
          .replaceExpression(exprA, removeParenthesis = true)
        infixExpr
          .asInstanceOf[ScInfixExpr]
          .getArgExpr
          .replaceExpression(exprB, removeParenthesis = true)

        val size =
          infixExpr
            .asInstanceOf[ScInfixExpr]
            .operation
            .nameId
            .getTextRange
            .getStartOffset - infixExpr.getTextRange.getStartOffset

        inWriteAction {
          methodCallExpr.replaceExpression(infixExpr, removeParenthesis = true)
          editor.getCaretModel.moveToOffset(start + diff + size)
          PsiDocumentManager
            .getInstance(project)
            .commitDocument(editor.getDocument)
        }
      case x =>
        throw new IllegalStateException(s"$text should be infix expression")
    }
  }
}
