package org.jetbrains.plugins.scala.performance.highlighting.projectHighlighting

import com.intellij.openapi.editor.LogicalPosition
import org.jetbrains.plugins.scala.SlowTests
import org.jetbrains.plugins.scala.performance.ScalaCommunityDownloadingAndImportingTestCase
import org.junit.experimental.categories.Category

/**
  * Author: Svyatoslav Ilinskiy
  * Date: 11/18/2015
  */
@Category(Array(classOf[SlowTests]))
class ScalaCommunityPerformanceRehighlightingTest
    extends RehighlightingPerformanceTypingTestBase
    with ScalaCommunityDownloadingAndImportingTestCase

  def testTypingInScalaPsiUtil(): Unit =
    doTest("ScalaPsiUtil.scala",
           4.seconds,
           Seq("val i = Some(10)\n"),
           new LogicalPosition(80, 1),
           Some("def foo() = {\n"))

  def testTypingInScalaPsiUtilInClassBody(): Unit =
    doTest("ScalaPsiUtil.scala",
           40.seconds,
           Seq("val i = Some(10)\n"),
           new LogicalPosition(80, 1),
           None)

  def testTypingInsideFunctionWithDefinedReturnType(): Unit =
    doTest("ScalaPsiUtil.scala",
           3.seconds,
           Seq("val i = Some(10)\n"),
           new LogicalPosition(80, 1),
           Some("def foo(): Unit = {\n"))
