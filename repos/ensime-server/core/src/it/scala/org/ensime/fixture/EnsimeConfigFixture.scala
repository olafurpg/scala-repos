// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import com.google.common.io.Files
import java.io.{ File => JFile }

import org.apache.commons.io.FileUtils.{ copyDirectory, copyFile }
import org.ensime.api._
import org.ensime.config._
import org.scalatest._
import org.ensime.util.file._

/**
 * Provides a fixture for tests to have access to a cloned project,
 * based on an example project that will be untouched.
 */
trait EnsimeConfigFixture {
  /** The definition of the original project to clone for testing. */
  def original: EnsimeConfig

  def copyTargets: Boolean = true

  def withEnsimeConfig(testCode: EnsimeConfig => Any): Any

  // convenience method
  def main(lang: String)(implicit config: EnsimeConfig): File =
    config.subprojects.head.sourceRoots.filter { dir =>
      val sep = JFile.separator
      dir.getPath.endsWith(s"${sep}main$sep$lang")
    }.head
  def scalaMain(implicit config: EnsimeConfig): File = main("scala")
  def javaMain(implicit config: EnsimeConfig): File = main("java")

  def mainTarget(implicit config: EnsimeConfig): File =
    config.subprojects.head.targets.head
}

object EnsimeConfigFixture {

  lazy val dotEnsime = File("../.ensime")
  if (!dotEnsime.exists) {
    System.err.println(
      "The .ensime file must exist to run the integration tests." +
        " Type 'sbt gen-ensime' to create it"
    )
    System.err.flush()
    sys.exit(1)
  }
  lazy val dotEnsimeCache = File("../.ensime_cache")
  dotEnsimeCache.mkdirs()

  lazy val EnsimeTestProject = EnsimeConfigProtocol.parse(dotEnsime.readString())

  // not completely empty, has a reference to the scala-library jar
  lazy val EmptyTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingEmpty"),
    javaLibs = Nil
  )
  lazy val SimpleTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingSimple")
  )
  lazy val SimpleJarTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingSimpleJar"),
    javaLibs = Nil
  )
  lazy val ImplicitsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingImplicits"),
    javaLibs = Nil
  )
  lazy val TimingTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingTiming"),
    javaLibs = Nil
  )
  lazy val DebugTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingDebug")
  )
  lazy val DocsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingDocs")
  )
  lazy val JavaTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingJava")
  )

  // generates an empty single module project in a temporary directory
  // and returns the config, containing many of the same settings
  // as the ensime-server project itself (source/dependency jars),
  // with options to copy ENSIME's own sources/classes into the structure.
  def cloneForTesting(
    source: EnsimeConfig,
    target: File,
    copyTargets: Boolean
  ): EnsimeConfig = {

    def rename(from: File): File = {
      val toPath = from.getAbsolutePath.replace(
        source.root.getAbsolutePath,
        target.getAbsolutePath
      )
      require(toPath != from.getAbsolutePath, s"${source.root.getAbsolutePath} ${target.getAbsolutePath} in ${from.getAbsolutePath}")
      File(toPath)
    }

    def renameAndCopy(from: File): File = {
      val to = rename(from)
      if (!to.isJar)
        copyDirectory(from, to)
      else
        copyFile(from, to)
      to
    }

    def renameAndCopyTarget(from: File): File =
      if (copyTargets) renameAndCopy(from)
      else rename(from)

    // I tried using shapeless everywhere here, but it OOMd the compiler :-(

    def cloneModule(m: EnsimeModule): EnsimeModule = m.copy(
      target = m.target.map(renameAndCopyTarget),
      targets = m.targets.map(renameAndCopyTarget),
      testTarget = m.testTarget.map(renameAndCopyTarget),
      testTargets = m.testTargets.map(renameAndCopyTarget),
      sourceRoots = m.sourceRoots.map(renameAndCopy)
    )

    val cacheDir = rename(source.cacheDir)
    cacheDir.mkdirs()
    val config = EnsimeConfigProtocol.validated(source.copy(
      rootDir = rename(source.rootDir),
      cacheDir = cacheDir,
      subprojects = source.subprojects.map(cloneModule)
    ))

    // HACK: we must force OS line endings on sources or the tests
    // (which have fixed points within the file) will fail on Windows
    config.scalaSourceFiles.foreach { file =>
      file.writeLines(file.readLines())
    }

    config
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned for every test in a suite.
 *
 * Implementations tend to run very slowly, so consider using
 * `SharedConfigFixture` if possible, or reducing your configuration
 * parameters to the bare minimal (e.g. remove JRE and dependencies to
 * index if not needed).
 */
trait IsolatedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture {
  //running in parallel actually slows things down
  //with ParallelTestExecution {
  import EnsimeConfigFixture._

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = withTempDir {
    dir => testCode(cloneForTesting(original, dir, copyTargets))
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned once for the test suite.
 */
trait SharedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture with BeforeAndAfterAll {
  import EnsimeConfigFixture._

  private val tmpDir = Files.createTempDir()

  private[fixture] var _config: EnsimeConfig = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _config = cloneForTesting(original, tmpDir, copyTargets)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    tmpDir.tree.reverse.foreach(_.delete())
  }

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = testCode(_config)

}
