package org.jetbrains.plugins.scala.lang.resolve2

/**
  * Pavel.Fatin, 02.02.2010
  */
class ImportAliasTest extends ResolveTestBase
  override def folderPath: String =
    super.folderPath + "import/alias/"

  //TODO importexclude
//  def testExclude = doTest
  //TODO importexclude
//  def testExcludeOnRename = doTest
  def testHide() = doTest()
  def testRename() = doTest()
