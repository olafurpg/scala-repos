// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec

class RefactoringHandlerSpec
    extends EnsimeSpec
    with IsolatedAnalyzerFixture
    with RichPresentationCompilerTestUtils {

  val encoding = "UTF-16"
  def original = EnsimeConfigFixture.EmptyTestProject.copy(
    compilerArgs = List("-encoding", encoding)
  )

  // transitionary methods
  def ContentsSourceFileInfo(file: File, contents: String) =
    SourceFileInfo(file, Some(contents))
  def ContentsInSourceFileInfo(file: File, contentsIn: File) =
    SourceFileInfo(file, contentsIn = Some(contentsIn))

  "RefactoringHandler" should "format files and preserve encoding" in {
    withAnalyzer { (config, analyzerRef) =>
      val file = srcFile(
        config,
        "abc.scala",
        contents(
          "package blah",
          "   class Something {",
          "def f(i:   Int) =1",
          " val x = (1\u21922)",
          "   }"
        ),
        write = true,
        encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      analyzer.handleFormatFiles(List(new File(file.path)))
      val fileContents = readSrcFile(file, encoding)

      val expectedContents = contents(
        "package blah",
        "class Something {",
        "  def f(i: Int) = 1",
        "  val x = (1 \u2192 2)",
        "}"
      )
      fileContents should ===(expectedContents)
    }
  }

  it should "format files from ContentsSourceFileInfo with handleFormatFile" in {
    withAnalyzer { (dir, analyzerRef) =>
      val content = contents(
        "package blah",
        "   class  Something   {}"
      )
      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(
        ContentsSourceFileInfo(new File("abc.scala"), content))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      formatted should ===(expectedContents)
    }
  }

  it should "format files from ContentsInSourceFileInfo with handleFormatFile and handle encoding" in {
    withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "package blah",
          "   class  Something   {}"
        ),
        write = true,
        encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(
        ContentsInSourceFileInfo(new File("abc.scala"), new File(file.path)))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      formatted should ===(expectedContents)
    }
  }

  it should "format files from FileSourceFileInfo with handleFormatFile and handle encoding" in {
    withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(
        dir,
        "abc.scala",
        contents(
          "package blah",
          "   class  Something   {}"
        ),
        write = true,
        encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val formatted =
        analyzer.handleFormatFile(SourceFileInfo(new File(file.path)))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      formatted === (expectedContents)
    }
  }

  it should "not format invalid files" in withAnalyzer {
    (config, analyzerRef) =>
      val file = srcFile(
        config,
        "abc.scala",
        contents(
          "package blah",
          "invalid scala syntax"
        ),
        write = true,
        encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      analyzer.handleFormatFiles(List(new File(file.path)))
      val fileContents = readSrcFile(file, encoding)

      val expectedContents = contents(
        "package blah",
        "invalid scala syntax"
      )
      fileContents should ===(expectedContents)
  }

  //
  // core/src/main/scala/org/ensime/core/Refactoring.scala#L.239
  //
  it should "organize imports when 3 imports exist" in withAnalyzer {
    (dir, analyzerRef) =>
      // Please refer Scala IDE
      // scala-refactoring/src/test/scala/scala/tools/refactoring/tests/implementations/imports/
      // --> OrganizeImportsWildcardsTest.scala

      // OrganizeImports need some paren --> "{...}"
      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "import java.lang.Integer.{valueOf => vo}",
          "import java.lang.Integer.toBinaryString",
          "import java.lang.String.valueOf",
          " ",
          "trait Temp {",
          "  valueOf(5)",
          "  vo(\"5\")",
          "  toBinaryString(27)",
          "}"
        ),
        write = true,
        encoding = encoding
      )

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      analyzer.handleRefactorPrepareRequest(
        new PrepareRefactorReq(
          procId,
          'Ignored,
          OrganiseImportsRefactorDesc(new File(file.path)),
          false
        )
      )
      analyzer.handleRefactorExec(
        new ExecRefactorReq(procId, RefactorType.OrganizeImports)
      )

      val formatted = readSrcFile(file, encoding)
      val expectedContents = contents(
        "import java.lang.Integer.{valueOf => vo, toBinaryString}",
        "import java.lang.String.valueOf",
        " ",
        "trait Temp {",
        "  valueOf(5)",
        "  vo(\"5\")",
        "  toBinaryString(27)",
        "}"
      )
      formatted should ===(expectedContents)
  }

  it should "add imports on the first line" in withAnalyzer {
    (dir, analyzerRef) =>
      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "import java.lang.Integer.toBinaryString",
          "import java.lang.String.valueOf",
          "",
          "trait Temp {",
          "  valueOf(5)",
          "  vo(\"5\")",
          "  toBinaryString(27)",
          "}"
        ),
        write = true,
        encoding = encoding
      )

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      analyzer.handleRefactorPrepareRequest(
        new PrepareRefactorReq(
          procId,
          'Ignored,
          AddImportRefactorDesc(
            "java.lang.Integer.{valueOf => vo}",
            new File(file.path)),
          false
        )
      )
      analyzer.handleRefactorExec(
        new ExecRefactorReq(procId, RefactorType.AddImport)
      )

      val formatted = readSrcFile(file, encoding)

      val expectedContents = contents(
        "import java.lang.Integer.toBinaryString",
        "import java.lang.String.valueOf",
        "import java.lang.Integer.{valueOf => vo}",
        "",
        "trait Temp {",
        "  valueOf(5)",
        "  vo(\"5\")",
        "  toBinaryString(27)",
        "}"
      )

      formatted should ===(expectedContents)
  }

  it should "add imports on the first line when other examples come" in {
    withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "package org.ensime.testing",
          "",
          "trait Temp {",
          "  valueOf(5)",
          "  vo(\"5\")",
          "  toBinaryString(27)",
          "}"
        ),
        write = true,
        encoding = encoding
      )

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      analyzer.handleRefactorPrepareRequest(
        new PrepareRefactorReq(
          procId,
          'Ignored,
          AddImportRefactorDesc("java.lang.Integer", new File(file.path)),
          false
        )
      )
      analyzer.handleRefactorExec(
        new ExecRefactorReq(procId, RefactorType.AddImport)
      )

      val formatted = readSrcFile(file, encoding)

      val expectedContents = contents(
        "package org.ensime.testing",
        "",
        "import java.lang.Integer",
        "trait Temp {",
        "  valueOf(5)",
        "  vo(\"5\")",
        "  toBinaryString(27)",
        "}"
      )

      formatted should ===(expectedContents)
    }
  }

  it should "rename a function id with params' opening/closing parenthesis on different lines" in withAnalyzer {
    (dir, analyzerRef) =>
      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "package org.ensime.testing",
          "trait Foo {",
          "def doIt(",
          ") = \"\"",
          "}",
          ""
        ),
        write = true,
        encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      analyzer.handleRefactorPrepareRequest(
        new PrepareRefactorReq(
          procId,
          'rename,
          RenameRefactorDesc("doItNow", new File(file.path), 43, 47),
          false
        )
      )
      analyzer.handleRefactorExec(
        new ExecRefactorReq(procId, RefactorType.Rename)
      )
      val formatted = readSrcFile(file, encoding)
      val expectedContents = contents(
        "package org.ensime.testing",
        "trait Foo {",
        "def doItNow(",
        ") = \"\"",
        "}",
        ""
      )
      formatted should ===(expectedContents)
  }

  it should "organize imports" in {
    withAnalyzer { (dir, analyzerRef) =>
      import org.ensime.util.file._

      //when 3 imports exist
      // "produce a diff file in the unified output format"

      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "import java.lang.Integer.{valueOf => vo}",
          "import java.lang.Integer.toBinaryString",
          "import java.lang.String.valueOf",
          " ",
          "trait Temp {",
          "  valueOf(5)",
          "  vo(\"5\")",
          "  toBinaryString(27)",
          "}"
        ),
        write = true,
        encoding = encoding
      )

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId,
          OrganiseImportsRefactorDesc(new File(file.path)),
          false
        )
      )
      val diffFile = result match {
        case RefactorDiffEffect(_, _, f) => f.canon
        case _                           => fail()
      }

      val sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
      val t = sdf.format(new Date((new File(file.path)).lastModified()))
      val diffContents = diffFile.readString()
      val expectedContents = s"""|--- ${file.path}	${t}
                                 |+++ ${file.path}	${t}
                                 |@@ -1,3 +1,2 @@
                                 |-import java.lang.Integer.{valueOf => vo}
                                 |-import java.lang.Integer.toBinaryString
                                 |+import java.lang.Integer.{valueOf => vo, toBinaryString}
                                 | import java.lang.String.valueOf
                                 |""".stripMargin

      diffContents should ===(expectedContents)
      diffFile.delete()
    }
  }

  it should "organize and group imports" in {
    withAnalyzer { (dir, analyzerRef) =>
      import org.ensime.util.file._

      val file = srcFile(
        dir,
        "tmp-contents",
        contents(
          "import scala._",
          "import java.lang.Integer",
          "import scala.Int",
          "import java._",
          " ",
          "trait Temp {",
          "  def i(): Int",
          "  def j(): Integer",
          "}",
          ""
        ),
        write = true,
        encoding = encoding
      )

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId,
          OrganiseImportsRefactorDesc(new File(file.path)),
          false
        )
      )
      val diffFile = result match {
        case RefactorDiffEffect(_, _, f) => f.canon
        case default                     => fail()
      }

      val sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
      val t = sdf.format(new Date((new File(file.path)).lastModified()))
      val diffContents = diffFile.readString()
      val expectedContents = s"""|--- ${file.path}	${t}
                                 |+++ ${file.path}	${t}
                                 |@@ -1,5 +1,5 @@
                                 |-import scala._
                                 |-import java.lang.Integer
                                 |-import scala.Int
                                 | import java._
                                 |+import java.lang.Integer
                                 |+
                                 |+import scala._
                                 |  \n""".stripMargin

      diffContents should ===(expectedContents)
      diffFile.delete()
    }
  }
}
