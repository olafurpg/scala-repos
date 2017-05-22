package org.jetbrains.plugins.scala.lang.resolve2

/**
  * Pavel.Fatin, 02.02.2010
  */
class FunctionCountClashTest extends ResolveTestBase
  override def folderPath: String =
    super.folderPath + "function/count/clash/"

//  def testEmptyAndNone = doTest
  def testOneAndEmpty() = doTest()
  def testOneAndNone() = doTest()
  def testOneAndTwo() = doTest()
