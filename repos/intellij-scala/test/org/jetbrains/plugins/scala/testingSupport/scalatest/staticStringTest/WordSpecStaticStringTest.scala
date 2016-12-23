package org.jetbrains.plugins.scala.testingSupport.scalatest.staticStringTest

import org.jetbrains.plugins.scala.testingSupport.scalatest.ScalaTestTestCase

/**
  * @author Roman.Shein
  * @since 26.06.2015.
  */
trait WordSpecStaticStringTest extends ScalaTestTestCase {
  val wordSpecClassName = "WordSpecStringTest"
  val wordSpecFileName = wordSpecClassName + ".scala"

  def addWordSpec() = {
    addFileToProject(
      wordSpecFileName,
      """
        |import org.scalatest._
        |
        |class WordSpecStringTest extends WordSpec {
        |  val constName = "const"
        |
        |  constName should {
        |    constName in {
        |    }
        |
        |    constName + " sum" in {
        |    }
        |  }
        |
        |  "sum " + "name" should {
        |    constName + constName in {
        |    }
        |
        |    "test" in {}
        |
        |    const + System.currentTimeMillis() in {
        |    }
        |  }
        |}
        |
      """.stripMargin.trim()
    )
  }

  def testWordSpecSum() = {
    addWordSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(17, 10, wordSpecFileName),
                             wordSpecClassName,
                             "sum name should test"))
  }

  def testWordSpecVal() = {
    addWordSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(6, 10, wordSpecFileName),
                             wordSpecClassName,
                             "const should const"))
  }

  def testWordSpecValSum() = {
    addWordSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(14, 10, wordSpecFileName),
                             wordSpecClassName,
                             "sum name should constconst"))
    assert(
      checkConfigAndSettings(createTestFromLocation(9, 10, wordSpecFileName),
                             wordSpecClassName,
                             "const should const sum"))
  }

  def testWordSpecNonConst() = {
    addWordSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(19, 10, wordSpecFileName),
                             wordSpecClassName))
  }
}
