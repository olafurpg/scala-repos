package org.jetbrains.plugins.scala.performance.typing

import java.io.File

import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.testFramework.PlatformTestUtil
import com.intellij.util.ThrowableRunnable
import org.jetbrains.plugins.scala.base.ScalaFixtureTestCase
import org.jetbrains.plugins.scala.util.TestUtils

/**
  * Author: Svyatoslav Ilinskiy
  * Date: 10/29/15.
  */
abstract class TypingTestWithPerformanceTestBase extends ScalaFixtureTestCase
  def doTest(stringsToType: List[String], timeoutInMillis: Int)
    val fileName = getTestName(true) + ".test"
    val filePath = folderPath + fileName
    val ioFile = new File(filePath)
    var fileText: String = FileUtil.loadFile(ioFile, CharsetToolkit.UTF8)
    fileText = StringUtil.convertLineSeparators(fileText)
    val (input, _) = separateText(fileText)
    myFixture.configureByText(fileName, input)
    PlatformTestUtil
      .startPerformanceTest("TypingTest" + getTestName(false),
                            timeoutInMillis,
                            new ThrowableRunnable[Nothing]
                              override def run(): Unit =
                                stringsToType.foreach(myFixture.`type`)
                            )
      .ioBound()
      .assertTiming()

  protected def folderPath: String = TestUtils.getTestDataPath + "/typing/"

  protected def separateText(fileText: String): (String, Option[String]) =
    fileText.indexOf("-----") match
      case -1 => (fileText, None)
      case other =>
        val (before, after) = fileText.splitAt(other)
        (before, Some(after.dropWhile(c => c == '-' || c == '\n')))
