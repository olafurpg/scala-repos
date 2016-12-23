package org.jetbrains.plugins.scala
package editor.smartEnter

import java.lang.Long

import com.intellij.codeInsight.CodeInsightUtil
import com.intellij.codeInsight.editorActions.smartEnter._
import com.intellij.codeInsight.lookup.LookupManager
import com.intellij.featureStatistics.FeatureUsageTracker
import com.intellij.openapi.actionSystem.IdeActions
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.actionSystem.EditorActionManager
import com.intellij.openapi.editor.ex.EditorEx
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.IncorrectOperationException
import org.jetbrains.plugins.scala.editor.smartEnter.fixers._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScPatternDefinition

/**
  * @author Dmitry.Naydanov
  * @author Ksenia.Sautina
  * @since 1/28/13
  */
object ScalaSmartEnterProcessor {
  private val LOG = Logger.getInstance(getClass)

  val myFixers = Seq(
    new ScalaMethodCallFixer,
    new ScalaIfConditionFixer,
    new ScalaForStatementFixer,
    new ScalaWhileConditionFixer,
    new ScalaMissingWhileBodyFixer,
    new ScalaMissingIfBranchesFixer,
    new ScalaMissingForBodyFixer
  )
  val myEnterProcessors = Seq.empty[EnterProcessor] //Can plug in later
}

class ScalaSmartEnterProcessor extends SmartEnterProcessor {
  private final val SMART_ENTER_TIMESTAMP: Key[Long] =
    Key.create("smartEnterOriginalTimestamp")

  def process(project: Project, editor: Editor, psiFile: PsiFile) = {
    FeatureUsageTracker.getInstance.triggerFeatureUsed(
      "codeassists.complete.statement")

    try {
      editor.putUserData(
        SMART_ENTER_TIMESTAMP,
        editor.getDocument.getModificationStamp.asInstanceOf[Long])
      processImpl(project, editor, psiFile)
    } finally {
      editor.putUserData(SMART_ENTER_TIMESTAMP, null)
    }

    true
  }

  private def processImpl(project: Project, editor: Editor, file: PsiFile) {
    try {
      commit(editor)

      val atCaret = getStatementAtCaret(editor, file)
      if (atCaret == null) return

      for (psiElement <- collectAllAtCaret(atCaret)) {
        for (fixer <- ScalaSmartEnterProcessor.myFixers) {
          val go = fixer.apply(editor, this, psiElement)

          if (LookupManager.getInstance(project).getActiveLookup != null)
            return //Lets dont spoil autocomplete

          go match {
            case fixer.WithEnter(inserted) =>
              commit(editor)
              if (inserted != 0) moveCaretBy(editor, inserted)
              plainEnter(editor)
              return
            case fixer.WithReformat(inserted) =>
              commit(editor)
              if (inserted != 0) moveCaretBy(editor, inserted)
              reformat(getStatementAtCaret(editor, file))
              return
            case _ =>
          }
        }
      }

      doEnter(atCaret, editor)
    } catch {
      case e: IncorrectOperationException =>
        ScalaSmartEnterProcessor.LOG.error(e.getMessage)
    }
  }

  protected override def reformat(caret: PsiElement) {
    if (caret == null) return

    var atCaret = caret
    val parent = atCaret.getParent

    parent match {
      case block: ScBlockExpr if block.exprs.headOption contains atCaret =>
        atCaret = block
      case forStmt: ScForStatement => atCaret = forStmt
      case _ =>
    }

    super.reformat(atCaret)
  }

  private def doEnter(caret: PsiElement, editor: Editor) {
    var atCaret = caret
    val psiFile = atCaret.getContainingFile
    val rangeMarker = createRangeMarker(atCaret)

    reformat(atCaret)
    commit(editor)

    atCaret = CodeInsightUtil.findElementInRange(psiFile,
                                                 rangeMarker.getStartOffset,
                                                 rangeMarker.getEndOffset,
                                                 atCaret.getClass)

    for (processor <- ScalaSmartEnterProcessor.myEnterProcessors) {
      if (atCaret != null &&
          processor.doEnter(editor, atCaret, isModified(editor))) return
    }

    if (!isModified(editor)) plainEnter(editor)
    else editor.getCaretModel.moveToOffset(rangeMarker.getEndOffset)
  }

  private def collectAllAtCaret(caret: PsiElement): Iterable[PsiElement] = {
    def doNotVisit(e: PsiElement) = e match {
      case _: PsiClass | _: PsiStatement | _: PsiMethod => true
      case _ => false
    }

    val buffer =
      scala.collection.mutable.ArrayBuffer((caret, doNotVisit(caret)))
    var idx = 0

    while (idx < buffer.size) {
      val (e, nonOk) = buffer(idx)
      val eNonOK = doNotVisit(e)

      if (e != null && (!eNonOK || !nonOk) && e.getChildren.nonEmpty)
        buffer ++= e.getChildren.zip(Stream continually eNonOK && nonOk)

      idx += 1
    }

    buffer.result().unzip._1
  }

  protected override def getStatementAtCaret(editor: Editor,
                                             psiFile: PsiFile): PsiElement = {
    val atCaret: PsiElement = super.getStatementAtCaret(editor, psiFile)
    if (atCaret.isInstanceOf[PsiWhiteSpace] || atCaret == null) return null
    if (("}" == atCaret.getText) &&
        !atCaret.getParent.isInstanceOf[PsiArrayInitializerExpression])
      return null

    var statementAtCaret: PsiElement = PsiTreeUtil.getParentOfType(
      atCaret,
      classOf[ScPatternDefinition],
      classOf[ScIfStmt],
      classOf[ScWhileStmt],
      classOf[ScForStatement],
      classOf[ScCatchBlock],
      classOf[ScMethodCall])

    if (statementAtCaret.isInstanceOf[PsiBlockStatement]) return null

    if (statementAtCaret != null &&
        statementAtCaret.getParent.isInstanceOf[ScForStatement]) {
      if (!PsiTreeUtil.hasErrorElements(statementAtCaret)) {
        statementAtCaret = statementAtCaret.getParent
      }
    }

    statementAtCaret match {
      case _: ScPatternDefinition | _: ScIfStmt | _: ScWhileStmt |
          _: ScForStatement | _: ScCatchBlock | _: ScMethodCall =>
        statementAtCaret
      case _ => null
    }
  }

  protected def moveCaretBy(editor: Editor, by: Int) {
    editor.getCaretModel.moveToOffset(editor.getCaretModel.getOffset + by)
  }

  protected def isUncommited(project: Project) =
    PsiDocumentManager.getInstance(project).hasUncommitedDocuments

  protected def plainEnter(editor: Editor) {
    getEnterHandler.execute(editor,
                            editor.getCaretModel.getCurrentCaret,
                            editor.asInstanceOf[EditorEx].getDataContext)
  }

  protected def getEnterHandler =
    EditorActionManager.getInstance.getActionHandler(
      IdeActions.ACTION_EDITOR_ENTER)

  protected def isModified(editor: Editor): Boolean = {
    val timestamp: Long = editor.getUserData(SMART_ENTER_TIMESTAMP)
    editor.getDocument.getModificationStamp != timestamp.longValue
  }
}
