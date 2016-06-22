package org.jetbrains.plugins.scala.lang.types.kindProjector

import java.io.File

import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.{CharsetToolkit, LocalFileSystem}
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScParameterizedTypeElement
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration

/**
  * Code taken mostly from ExistentialSimplificationTestBase, with minor modifications
  */
abstract class KindProjectorTestBase
    extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  private val startExprMarker = "/*start*/"
  private val endExprMarker = "/*end*/"

  def folderPath: String = baseRootPath() + "types/kindProjector/"

  override protected def setUp() = {
    super.setUp()

    val defaultProfile =
      ScalaCompilerConfiguration.instanceIn(getProjectAdapter).defaultProfile
    val newSettings = defaultProfile.getSettings
    newSettings.plugins = newSettings.plugins :+ "kind-projector"
    defaultProfile.setSettings(newSettings)
  }

  protected def doTest() {
    import _root_.junit.framework.Assert._

    val filePath = folderPath + getTestName(false) + ".scala"
    val file = LocalFileSystem.getInstance.findFileByPath(
        filePath.replace(File.separatorChar, '/'))
    assert(file != null, "file " + filePath + " not found")
    val fileText = StringUtil.convertLineSeparators(
        FileUtil.loadFile(new File(file.getCanonicalPath),
                          CharsetToolkit.UTF8))
    configureFromFileTextAdapter(getTestName(false) + ".scala", fileText)
    val scalaFile = getFileAdapter.asInstanceOf[ScalaFile]
    val offset = fileText.indexOf(startExprMarker)
    val startOffset = offset + startExprMarker.length

    assert(
        offset != -1,
        "Not specified start marker in test case. Use /*start*/ in scala file for this.")
    val endOffset = fileText.indexOf(endExprMarker)
    assert(
        endOffset != -1,
        "Not specified end marker in test case. Use /*end*/ in scala file for this.")

    val expr: ScParameterizedTypeElement =
      PsiTreeUtil.findElementOfClassAtRange(
          scalaFile,
          startOffset,
          endOffset,
          classOf[ScParameterizedTypeElement])
    assert(expr != null, "Not specified expression in range to infer type.")
    val typez = expr.computeDesugarizedType
    typez match {

      case Some(tp) =>
        val res = tp.getText
        val lastPsi = scalaFile.findElementAt(scalaFile.getText.length - 1)
        val text = lastPsi.getText
        val output = lastPsi.getNode.getElementType match {
          case ScalaTokenTypes.tLINE_COMMENT => text.substring(2).trim
          case ScalaTokenTypes.tBLOCK_COMMENT | ScalaTokenTypes.tDOC_COMMENT =>
            text.substring(2, text.length - 2).trim
          case _ =>
            assertTrue("Test result must be in last comment statement.", false)
        }
        assertEquals(output, res)
      case _ =>
        assert(assertion = false,
               message = "Projection type not created from parameterized type")
    }
  }
}
