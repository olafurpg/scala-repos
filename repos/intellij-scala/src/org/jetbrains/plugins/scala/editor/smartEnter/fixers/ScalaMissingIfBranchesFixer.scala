package org.jetbrains.plugins.scala
package editor.smartEnter.fixers

import com.intellij.openapi.editor.Editor
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.editor.smartEnter.ScalaSmartEnterProcessor
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockExpr, ScIfStmt}

/**
  * @author Dmitry.Naydanov
  * @author Ksenia.Sautina
  * @since 1/31/13
  */
class ScalaMissingIfBranchesFixer extends ScalaFixer
  def apply(editor: Editor,
            processor: ScalaSmartEnterProcessor,
            psiElement: PsiElement): OperationPerformed =
    val ifStatement =
      PsiTreeUtil.getParentOfType(psiElement, classOf[ScIfStmt], false)
    if (ifStatement == null) return NoOperation

    val doc = editor.getDocument
    var transformingOneLiner = false
    val thenBranch = ifStatement.thenBranch

    ifStatement.thenBranch match
      case Some(block: ScBlockExpr) =>
        ifStatement.condition.foreach
          case cond =>
            if (cond.getTextRange.containsOffset(
                    editor.getCaretModel.getOffset))
              return placeInWholeBlock(block, editor)
        return NoOperation
      case Some(branch)
          if startLine(doc, branch) == startLine(doc, ifStatement) =>
        if (ifStatement.condition.isDefined) return NoOperation
        transformingOneLiner = true
      case _ =>

    val rParenth = ifStatement.getRightParenthesis.orNull
    assert(rParenth != null)

    val rParenthOffset = rParenth.getTextRange.getEndOffset

    if (ifStatement.elseBranch.isEmpty && !transformingOneLiner ||
        ifStatement.thenBranch.isEmpty)
      doc.insertString(rParenthOffset, " {}")
    else
      doc.insertString(rParenthOffset, " {")
      doc.insertString(thenBranch.get.getTextRange.getEndOffset + 1, "}")

    editor.getCaretModel.moveToOffset(rParenthOffset)

    WithEnter(2)
