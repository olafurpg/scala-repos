package org.jetbrains.plugins.scala
package lang.actions.editor

import java.io.File

import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.editor.typedHandler.ScalaTypedHandler
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.util.TestUtils

/**
  * User: Dmitry.Naydanov
  * Date: 10.07.14.
  */
class ArrowTypingTest extends ScalaLightCodeInsightFixtureTestAdapter {
  private var settings: ScalaCodeStyleSettings = null

  private def convertLoadedString(str: String) =
    str
      .replace("$CARET_MARKER", CARET_MARKER)
      .replace("${ScalaTypedHandler.unicodeCaseArrow}",
               ScalaTypedHandler.unicodeCaseArrow)

  override def getTestDataPath: String =
    TestUtils.getTestDataPath + "/actions/editor/arrows"

  override protected def setUp(): Unit = {
    super.setUp()

    settings = ScalaCodeStyleSettings.getInstance(myFixture.getProject)
  }

  def testReplaceCaseArrow() {
    settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = true

    val before = s"""
         | 123 match {
         |  case 321 =$CARET_MARKER
         | }
       """.stripMargin

    val after = s"""
         | 123 match {
         |  case 321 ${ScalaTypedHandler.unicodeCaseArrow}$CARET_MARKER
         | }
       """.stripMargin

    checkGeneratedTextAfterTyping(before, after, '>')
  }

  def testReplaceFunTypeArrow() {
    settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = true

    val before = s"""
         |val b: Int =$CARET_MARKER
       """.stripMargin

    val after = s"""
         |val b: Int ${ScalaTypedHandler.unicodeCaseArrow}$CARET_MARKER
       """.stripMargin

    checkGeneratedTextAfterTyping(before, after, '>')
  }

  def testReplaceLambdaArrow() {
    settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = true

    val before1 = convertLoadedString(
        FileUtil.loadFile(
            new File(getTestDataPath + s"/${getTestName(true)}Before.test")))
    val after1 = convertLoadedString(
        FileUtil.loadFile(
            new File(getTestDataPath + s"/${getTestName(true)}After.test")))

    checkGeneratedTextAfterTyping(before1, after1, '>')
  }

  def testReplaceMapArrow() {
    settings.REPLACE_MAP_ARROW_WITH_UNICODE_CHAR = true

    val before = s"""
         |val map = Map(a -$CARET_MARKER)
       """.stripMargin

    val after = s"""
         |val map = Map(a ${ScalaTypedHandler.unicodeMapArrow}$CARET_MARKER)
       """.stripMargin

    checkGeneratedTextAfterTyping(before, after, '>')
  }

  def testReplaceForGeneratorArrow() {
    settings.REPLACE_FOR_GENERATOR_ARROW_WITH_UNICODE_CHAR = true

    val before = s"""
         | for (j <$CARET_MARKER )
       """.stripMargin

    val after = s"""
         | for (j ${ScalaTypedHandler.unicodeForGeneratorArrow}$CARET_MARKER )
       """.stripMargin

    checkGeneratedTextAfterTyping(before, after, '-')
  }

  def testDontAddGtSign() {
    settings.REPLACE_CASE_ARROW_WITH_UNICODE_CHAR = true

    val fileName = s"$getTestDataPath/${getTestName(true)}.test"
    val text = convertLoadedString(FileUtil.loadFile(new File(fileName)))

    checkGeneratedTextAfterTyping(text, text, '>')
  }
}
