package org.jetbrains.plugins.scala.testingSupport.scalatest.staticStringTest

import org.jetbrains.plugins.scala.testingSupport.scalatest.ScalaTestTestCase

/**
  * @author Roman.Shein
  * @since 26.06.2015.
  */
trait FreeSpecStaticStringTest extends ScalaTestTestCase {
  val freeSpecClassName = "FreeSpecStringTest"
  val freeSpecFileName = freeSpecClassName + ".scala"

  def addFreeSpec() {
    addFileToProject(
      freeSpecFileName,
      """
        |import org.scalatest._
        |
        |class FreeSpecStringTest extends FreeSpec {
        |  val constName = " consts"
        |  val otherConstName = "Const name"
        |  val innerConst = "InnerConst"
        |
        |  "A" + " FreeSpecTest" - {
        |    "should work with sum" in {
        |    }
        |
        |    "should work with sum of" + constName in {
        |    }
        |  }
        |
        |  otherConstName - {
        |    "innerNonConst" in {
        |    }
        |
        |    innerConst in {
        |    }
        |  }
        |
        |  "base " + foo() - {
        |    "unreachable" in {
        |    }
        |  }
        |}
      """.stripMargin.trim()
    )
  }

  def testFreeSpecSum() = {
    addFreeSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(8, 7, freeSpecFileName),
                             freeSpecClassName,
                             "A FreeSpecTest should work with sum"))
  }

  def testFreeSpecVal() = {
    addFreeSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(16, 7, freeSpecFileName),
                             freeSpecClassName,
                             "Const name innerNonConst"))
    assert(
      checkConfigAndSettings(createTestFromLocation(19, 7, freeSpecFileName),
                             freeSpecClassName,
                             "Const name InnerConst"))
  }

  def testFreeSpecValSum() = {
    addFreeSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(11, 7, freeSpecFileName),
                             freeSpecClassName,
                             "A FreeSpecTest should work with sum of consts"))
  }

  def testFreeSpecNonConst() = {
    addFreeSpec()

    assert(
      checkConfigAndSettings(createTestFromLocation(24, 7, freeSpecFileName),
                             freeSpecClassName))
  }
}
