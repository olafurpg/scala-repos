package org.jetbrains.plugins.scala
package editor.smartEnter.fixers

import com.intellij.openapi.editor.Editor
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.editor.smartEnter.ScalaSmartEnterProcessor
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScForStatement

/**
  * @author Dmitry.Naydanov
  * @author Ksenia.Sautina
  * @since 2/5/13
  */
class ScalaMissingForBodyFixer extends ScalaFixer
  def apply(editor: Editor,
            processor: ScalaSmartEnterProcessor,
            psiElement: PsiElement): OperationPerformed =
    val forStatement =
      PsiTreeUtil.getParentOfType(psiElement, classOf[ScForStatement], false)
    if (forStatement == null) return NoOperation

    val doc = editor.getDocument

    forStatement.body match
      case None =>
        val (eltToInsertAfter, text) = forStatement.getRightParenthesis match
          case None => (forStatement, ") {}")
          case Some(parenth) =>
            moveToEnd(editor, parenth)
            (parenth, " {}")

        doc.insertString(eltToInsertAfter.getTextRange.getEndOffset, text)
        WithEnter(text.length - 1)
      case Some(_) => NoOperation
