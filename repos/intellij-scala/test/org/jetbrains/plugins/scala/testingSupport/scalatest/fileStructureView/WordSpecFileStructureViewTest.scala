package org.jetbrains.plugins.scala.testingSupport.scalatest.fileStructureView

import org.jetbrains.plugins.scala.lang.structureView.elements.impl.TestStructureViewElement._
import org.jetbrains.plugins.scala.testingSupport.scalatest.ScalaTestTestCase

/**
  * @author Roman.Shein
  * @since 21.04.2015.
  */
trait WordSpecFileStructureViewTest extends ScalaTestTestCase
  private val className = "WordSpecViewTest"

  def addWordSpecViewTest(): Unit =
    addFileToProject(className + ".scala",
                     """
        |import org.scalatest._
        |
        |class WordSpecViewTest extends WordSpec {
        |  "parent1" should {
        |    "child1" in {}
        |
        |    "child2" when {}
        |
        |    "pending1" in pending
        |
        |    "pending2" is pending
        |  }
        |
        |  "parent2" which {
        |    "child3" must {}
        |    "child4" can {}
        |    "ignore1" ignore {}
        |    "ignore2" ignore pending
        |  }
        |}
      """.stripMargin)

  def testWordSpecNormal(): Unit =
    addWordSpecViewTest()
    runFileStructureViewTest(className,
                             normalStatusId,
                             "\"parent1\"",
                             "\"child1\"",
                             "\"child2\"",
                             "\"parent2\"",
                             "\"child3\"",
                             "\"child4\"")

  def testWordSpecHierarchy(): Unit =
    addWordSpecViewTest()
    runFileStructureViewTest(className, "\"child1\"", Some("\"parent1\""))
    runFileStructureViewTest(className, "\"child2\"", Some("\"parent1\""))
    runFileStructureViewTest(className, "\"child3\"", Some("\"parent2\""))
    runFileStructureViewTest(className, "\"child4\"", Some("\"parent2\""))

  def testWordSpecIgnored(): Unit =
    addWordSpecViewTest()
    runFileStructureViewTest(className, ignoredStatusId, "\"ignore1\"")

  def testWordSpecPending(): Unit =
    addWordSpecViewTest()
    runFileStructureViewTest(className, pendingStatusId, "\"pending1\"")
    runFileStructureViewTest(className, pendingStatusId, "\"pending2\"")

  def testWordSpecIgnoredAndPending(): Unit =
    addWordSpecViewTest()
    runFileStructureViewTest(className, ignoredStatusId, "\"ignore2\"")
