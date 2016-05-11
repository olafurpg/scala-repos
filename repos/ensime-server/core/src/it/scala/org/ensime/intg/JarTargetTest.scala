// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import org.apache.commons.io.FileUtils
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import scala.concurrent.duration._

/**
 * Tests a project that uses jars instead of classfiles in the target.
 */
class JarTargetTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.SimpleJarTestProject

  "ensime-server" should "index jar targets" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        withProject { (project, asyncHelper) =>
          import tk._

          mainTarget should be a 'file

          eventually(interval(1 second)) {
            project ! PublicSymbolSearchReq(List("Foo"), 5)
            atLeast(1, expectMsgType[SymbolSearchResults].syms) should matchPattern {
              case TypeSearchResult("baz.Foo$", "Foo$", DeclaredAs.Class, Some(_)) =>
            }
          }
        }
      }
    }
  }

  it should "allow jar targets to be deleted" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        withProject { (project, asyncHelper) =>
          mainTarget should be a 'file

          // no scaling here
          eventually(timeout(30 seconds), interval(1 second)) {
            mainTarget.delete() shouldBe true
          }
        }
      }
    }
  }

}

/**
 * Variant of JarTargetTest with jars missing on startup.
 */
class MissingJarTargetTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.SimpleJarTestProject

  override def copyTargets = false

  "ensime-server" should "index jar targets that appear after startup" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit tk =>
        withProject { (project, asyncHelper) =>
          import tk._

          // internal consistency check
          mainTarget(original) should be a 'file

          // we want to support the case where the .jar doesn't
          // exist on startup, and we don't try to create it.
          mainTarget should not be 'exists

          FileUtils.copyFile(mainTarget(original), mainTarget)
          mainTarget should be a 'file

          // means the file addition was detected
          asyncHelper.expectMsg(CompilerRestartedEvent)

          eventually(interval(1 second)) {
            project ! PublicSymbolSearchReq(List("Foo"), 5)
            atLeast(1, expectMsgType[SymbolSearchResults].syms) should matchPattern {
              case TypeSearchResult("baz.Foo$", "Foo$", DeclaredAs.Class, Some(_)) =>
            }
          }
        }
      }
    }
  }

}
