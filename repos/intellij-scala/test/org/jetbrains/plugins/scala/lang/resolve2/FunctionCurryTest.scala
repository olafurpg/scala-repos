package org.jetbrains.plugins.scala.lang.resolve2

/**
  * Pavel.Fatin, 02.02.2010
  */
class FunctionCurryTest extends ResolveTestBase
  override def folderPath: String =
    super.folderPath + "function/curry/"

  def testCurryiedToCurryied() = doTest()
  def testCurryiedToNormal() = doTest()
  def testNormalToCurryied() = doTest()
