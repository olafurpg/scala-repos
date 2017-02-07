package org.jetbrains.plugins.scala
package worksheet.interactive

import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.event.{
  DocumentAdapter,
  DocumentEvent,
  DocumentListener
}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.newvfs.FileAttribute
import com.intellij.problems.WolfTheProblemSolver
import com.intellij.psi.{PsiDocumentManager, PsiFile}
import com.intellij.util.Alarm
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.plugins.scala.settings.ScalaProjectSettings
import org.jetbrains.plugins.scala.worksheet.actions.RunWorksheetAction
import org.jetbrains.plugins.scala.worksheet.processor.{
  FileAttributeUtilCache,
  WorksheetPerFileConfig
}
import org.jetbrains.plugins.scala.worksheet.server.WorksheetProcessManager

/**
  * User: Dmitry.Naydanov
  * Date: 01.04.14.
  */
object WorksheetAutoRunner extends WorksheetPerFileConfig {
  val RUN_DELAY_MS_MAXIMUM = 3000
  val RUN_DELAY_MS_MINIMUM = 700

  private val AUTORUN = new FileAttribute("ScalaWorksheetAutoRun", 1, true)

  def isSetEnabled(file: PsiFile): Boolean =
    FileAttributeUtilCache.readAttribute(AUTORUN, file) contains enabled

  def isSetDisabled(file: PsiFile): Boolean =
    FileAttributeUtilCache.readAttribute(AUTORUN, file) contains disabled

  def setAutorun(file: PsiFile, autorun: Boolean) {
    setEnabled(file, AUTORUN, autorun)
  }

  def getInstance(project: Project) =
    project.getComponent(classOf[WorksheetAutoRunner])
}

class WorksheetAutoRunner(project: Project, woof: WolfTheProblemSolver)
    extends ProjectComponent {
  private val listeners =
    ContainerUtil.createConcurrentWeakMap[Document, DocumentListener]()
  private val myAlarm = new Alarm(Alarm.ThreadToUse.SWING_THREAD, project)
  private lazy val settings = ScalaProjectSettings.getInstance(project)

  override def disposeComponent() {}

  override def initComponent() {}

  override def projectClosed() {}

  override def projectOpened() {}

  override def getComponentName: String = "WorksheetAutoRunner"

  def getAutoRunDelay =
    ScalaProjectSettings.getInstance(project).getAutoRunDelay

  def addListener(document: Document) {
    if (listeners.get(document) == null) {
      val listener = new MyDocumentAdapter(document)

      document addDocumentListener listener
      listeners.put(document, listener)
    }
  }

  def removeListener(document: Document) {
    val listener = listeners.remove(document)
    if (listener != null) document removeDocumentListener listener
  }

  private class MyDocumentAdapter(document: Document) extends DocumentAdapter {
    val documentManager = PsiDocumentManager.getInstance(project)

    @inline private def isDisabledOn(file: PsiFile) = {
      WorksheetAutoRunner.isSetDisabled(file) || !settings.isInteractiveMode &&
      !WorksheetAutoRunner.isSetEnabled(file)
    }

    override def documentChanged(e: DocumentEvent) {
      if (project.isDisposed) return

      val psiFile = documentManager.getPsiFile(document)
      if (isDisabledOn(psiFile)) return

      val virtualFile = psiFile.getVirtualFile
      myAlarm.cancelAllRequests()

      if (woof.hasSyntaxErrors(virtualFile) ||
          WorksheetProcessManager.running(virtualFile)) return

      myAlarm.addRequest(
        new Runnable {
          override def run() {
            if (!woof.hasSyntaxErrors(virtualFile) &&
                !WorksheetProcessManager.running(virtualFile))
              RunWorksheetAction.runCompiler(project, auto = true)
          }
        },
        getAutoRunDelay,
        true
      )
    }
  }
}
