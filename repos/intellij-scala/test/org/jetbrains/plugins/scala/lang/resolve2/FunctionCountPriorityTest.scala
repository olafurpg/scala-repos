package org.jetbrains.plugins.scala.lang.resolve2

/**
  * Pavel.Fatin, 02.02.2010
  */
class FunctionCountPriorityTest extends ResolveTestBase
  override def folderPath: String =
    super.folderPath + "function/count/priority/"

  def testEmptyToAll() = doTest()
  def testNoneToAll() = doTest()
  def testOneToAll() = doTest()
  def testTwoToAll() = doTest()
