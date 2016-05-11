package org.jetbrains.plugins.scala.lang.optimize
package generated

class OptimizeImportsSimpleTest extends OptimizeImportsTestBase {
  //This class was generated by build script, please don't change this
  override def folderPath: String = super.folderPath + "simple/"

  protected override def rootPath(): String = folderPath

  def testSorted() = doTest()

  def testSortedInPackage() = doTest()

  def testTwoExpressions() = doTest()

  def testDeleteBraces() = doTest()

  def testDontSaveNotResolved() = doTest()

  def testImportChainUsed() = doTest()

  def testLanguageFeatures() = doTest()

  def testNewLines() = doTest()

  def testOneImport() = doTest()

  def testScalaDoc(): Unit = doTest()

  def testSCL7275(): Unit = doTest()

  def testSomeTrait() = doTest()

  def testUnusedImportChain() = doTest()

  def testUnusedSelector() = doTest()

  def testUsedImport() = doTest()

  def testRelativeNameConflict() = doTest()
}
