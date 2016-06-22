package org.jetbrains.plugins.scala.testingSupport.scalatest.fileStructureView

import org.jetbrains.plugins.scala.lang.structureView.elements.impl.TestStructureViewElement._
import org.jetbrains.plugins.scala.testingSupport.scalatest.ScalaTestTestCase
import org.jetbrains.plugins.scala.testingSupport.test.structureView.TestNodeProvider

/**
  * @author Roman.Shein
  * @since 20.04.2015.
  */
trait FreeSpecFileStructureViewTest extends ScalaTestTestCase {
  private val className = "FreeSpecViewTest"

  def addFreeSpecViewTest(): Unit = {
    addFileToProject(className + ".scala",
                     """
        |import org.scalatest._
        |
        |class FreeSpecViewTest extends FreeSpec {
        |  "level1" - {
        |    "level1_1" in {}
        |
        |    "level1_2" - {
        |      "level1_2_1" in {}
        |    }
        |
        |    "level1_2" is pending
        |
        |    "level1_3" in pending
        |  }
        |
        |  "level2" ignore {
        |    "level2_1" in {}
        |
        |    "level2_2" ignore {}
        |  }
        |
        |  "level3" ignore pending
        |}
      """.stripMargin)
  }

  def testFreeSpecNormal(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(className,
                             normalStatusId,
                             "\"level1\"",
                             "\"level1_1\"",
                             "\"level1_2\"",
                             "\"level1_2_1\"")
  }

  def testFreeSpecHierarchy(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(className, "\"level1_1\"", Some("\"level1\""))
    runFileStructureViewTest(className, "\"level1_2_1\"", Some("\"level1_2\""))
  }

  def testFreeSpecIgnoredHierarchy(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(
        className,
        "\"level2_1\"",
        Some("\"level2\"" + TestNodeProvider.ignoredSuffix))
    runFileStructureViewTest(
        className,
        "\"level2_2\"" + TestNodeProvider.ignoredSuffix,
        Some("\"level2\"" + TestNodeProvider.ignoredSuffix),
        ignoredStatusId)
  }

  def testFreeSpecIgnored(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(className,
                             ignoredStatusId,
                             "\"level2\"",
                             "\"level2_2\"")
  }

  def testFreeSpecIgnoredAndPending(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(className, ignoredStatusId, "\"level3\"")
  }

  def testFreeSpecPending(): Unit = {
    addFreeSpecViewTest()
    runFileStructureViewTest(className,
                             pendingStatusId,
                             "\"level1_2\"",
                             "\"level1_3\"")
  }
}
