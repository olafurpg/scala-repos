package org.jetbrains.plugins.scala
package editor.smartEnter.fixers

import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.psi.{PsiWhiteSpace, PsiErrorElement, PsiElement}
import org.jetbrains.plugins.scala.editor.smartEnter.ScalaSmartEnterProcessor
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlockExpr

/**
  * @author Dmitry.Naydanov
  * @author Ksenia.Sautina
  * @since 1/28/13
  */
trait ScalaFixer {
  def apply(editor: Editor,
            processor: ScalaSmartEnterProcessor,
            psiElement: PsiElement): OperationPerformed

  protected def startLine(doc: Document, psiElement: PsiElement): Int =
    doc.getLineNumber(psiElement.getTextRange.getStartOffset)

  protected def startLine(editor: Editor, psiElement: PsiElement): Int =
    startLine(editor.getDocument, psiElement)

  protected def moveToStart(editor: Editor, psiElement: PsiElement) =
    editor.getCaretModel.moveToOffset(psiElement.getTextRange.getStartOffset)

  protected def moveToEnd(editor: Editor, psiElement: PsiElement) =
    editor.getCaretModel.moveToOffset(psiElement.getTextRange.getEndOffset)

  protected def getOffsetOrParent(parent: PsiElement, child: PsiElement): Int = {
    var s = child.getNextSibling

    while (s != null) {
      s match {
        case error: PsiErrorElement => return error.getTextRange.getEndOffset
        case wsn: PsiWhiteSpace if wsn.textContains('\n') =>
          return wsn.getTextRange.getStartOffset
        case _ =>
      }

      s = s.getNextSibling
    }

    parent.getTextRange.getEndOffset
  }

  protected def placeInWholeBlock(block: ScBlockExpr,
                                  editor: Editor): OperationPerformed = {
    @inline def move2start(psi: PsiElement) =
      editor.getCaretModel.moveToOffset(psi.getTextRange.getStartOffset)
    @inline def move2end(psi: PsiElement) =
      editor.getCaretModel.moveToOffset(psi.getTextRange.getEndOffset)

    if (block.exprs.nonEmpty) {
      move2end(block.getFirstChild)
      return WithEnter(0)
    }

    block.getRBrace match {
      case Some(brace) =>
        block.getFirstChild match {
          case l: PsiElement
              if l.getNode.getElementType == ScalaTokenTypes.tLBRACE =>
            val r = brace.getPsi

            l.getNextSibling match {
              case s if s == r =>
                move2end(l)
                WithEnter(0)
              case ws: PsiWhiteSpace if ws.getNextSibling == r =>
                move2start(l)
                editor.getDocument.replaceString(l.getTextRange.getStartOffset,
                                                 r.getTextRange.getEndOffset,
                                                 "{}")
                WithEnter(1)
              case other =>
                move2end(other)
                WithReformat(0)
            }

          case _ =>
            move2start(block)
            WithReformat(0)
        }
      case _ =>
        block.lastChild.foreach(move2start)
        WithReformat(0)
    }
  }

  trait OperationPerformed
  case class WithEnter(moveBy: Int) extends OperationPerformed
  case class WithReformat(moveBy: Int) extends OperationPerformed
  case object NoOperation extends OperationPerformed
}
