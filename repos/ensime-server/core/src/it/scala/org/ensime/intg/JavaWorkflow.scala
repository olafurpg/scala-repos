// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._

// a pure java project, checking that how things behave without scala
class JavaWorkflow extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.JavaTestProject

  "ensime-server" should "open the pure Java test project" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          import testkit._

          val sourceRoot = javaMain(config)
          val fooFile = sourceRoot / "pure/NoScalaHere.java"
          val fooFilePath = fooFile.getAbsolutePath

          project ! TypecheckFilesReq(List(Left(fooFile)))
          expectMsg(VoidResponse)

          project ! TypeAtPointReq(Left(fooFile), OffsetRange(30))
          expectMsg(BasicTypeInfo("pure.NoScalaHere", DeclaredAs.Class, "pure.NoScalaHere", Nil, Nil, Some(EmptySourcePosition())))
        }
      }
    }
  }
}
