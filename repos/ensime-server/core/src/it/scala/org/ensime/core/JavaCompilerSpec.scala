// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File

import org.ensime.api.OffsetSourcePosition
import org.ensime.api.LineSourcePosition
import org.ensime.api.SourceFileInfo
import org.ensime.core.javac.JavaFqn
import org.ensime.fixture._
import org.ensime.indexer.SearchServiceTestUtils
import org.ensime.util.EnsimeSpec

class JavaCompilerSpec extends EnsimeSpec
    with IsolatedJavaCompilerFixture
    with SearchServiceTestUtils {

  val original = EnsimeConfigFixture.SimpleTestProject

  "JavaCompiler" should "generate compilation notes" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "class Test1 {",
        "  ksjdfkdjsf @1@",
        "}") { (sf, p, label, cc) =>
        }
      store.notes should not be empty
    }
  }

  it should "find type at point" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "class Tes@0@t1 {",
        "  private void main() {",
        "    int fo@1@o = 1;",
        "    System.out.println(fo@2@o);",
        "  }",
        "}") { (sf, offset, label, cc) =>
          val info = cc.askTypeAtPoint(sf, offset).get
          label match {
            case "0" => info.name shouldBe "Test1"
            case "1" => info.name shouldBe "int"
            case "2" => info.name shouldBe "int"
          }
        }
    }
  }

  it should "link symbols to their source positions" in {
    withJavaCompiler { (_, config, cc, store, _) =>
      val test1 = SourceFileInfo(new File(config.rootDir, "testing/simple/src/main/java/org/example/Test1.java"))
      val test2 = SourceFileInfo(new File(config.rootDir, "testing/simple/src/main/java/org/example/Test2.java"))

      cc.askLinkPos(JavaFqn("org.example", "Test2", None), test2) should matchPattern { case Some(OffsetSourcePosition(f, 22)) => }
      cc.askLinkPos(JavaFqn("org.example", "Foo", None), test2) should matchPattern { case None => }
      cc.askLinkPos(JavaFqn("org.example", "Test2.Bar", None), test2) should matchPattern { case Some(OffsetSourcePosition(f, 260)) => }
      //    cc.askLinkPos(JavaFqn("org.example", "Test2", Some("compute()")), test2) should matchPattern { case Some(OffsetSourcePosition(f, 58)) => }

    }
  }

  it should "find symbol at point" in withJavaCompiler { (_, config, cc, store, search) =>
    implicit val searchService = search
    refresh()
    runForPositionInCompiledSource(config, cc,
      "package org.example;",
      "import java.io.File;",
      "class Test1 {",
      "  private class Foo { public Foo() {} }",
      "  public static final int CONST = 2;",
      "  private void main(String[] args) {",
      "    int foo = 1;",
      "    System.out.println(ar@1@gs);",
      "    System.out.pr@3@intln(new Fo@2@o());",
      "    System.out.println(new Fi@4@le(\".\"));",
      "    System.out.println(Tes@5@t2.com@6@pute());",
      "    System.out.println(comp@7@ute(2, 3));",
      "    System.out.println(CO@8@NST);",
      "    System.out.println(@11@fo@0@o@12@);",
      "    int k = 2;",
      "    System.out.println( @13@k@14@ );",
      "  }",
      "  private static int compute(int a, int b) {",
      "    return a + b;",
      "  }",
      "  private static String hello(D@9@ay day) {",
      "    if (day == Day.MO@10@N) return \"monday\";",
      "    return \"tues\";",
      "  }",
      "  public enum Day { MON, TUES }",
      "}") { (sf, offset, label, cc) =>
        val info = cc.askSymbolAtPoint(sf, offset).get
        label match {
          case "0" | "11" | "12" =>
            info.name shouldBe "foo"
            info.localName shouldBe "foo"
            info.`type`.name shouldBe "int"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 174)) if f.getName == "Test1.java" => }
          case "1" =>
            info.name shouldBe "args"
            info.localName shouldBe "args"
            info.`type`.name shouldBe "java.lang.String[]"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 153)) if f.getName == "Test1.java" => }
          case "2" =>
            info.name shouldBe "org.example.Test1.Foo"
            info.localName shouldBe "Foo"
            info.`type`.name shouldBe "org.example.Test1.Foo"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 58)) if f.getName == "Test1.java" => }
          case "3" =>
            info.name shouldBe "java.io.PrintStream.println(java.lang.Object)"
            info.localName shouldBe "println"
            info.`type`.name shouldBe "(java.lang.Object)void"
            info.isCallable shouldBe true
          case "4" =>
            info.name shouldBe "java.io.File"
            info.localName shouldBe "File"
            info.`type`.name shouldBe "java.io.File"
            info.isCallable shouldBe false
          case "5" =>
            info.name shouldBe "org.example.Test2"
            info.localName shouldBe "Test2"
            info.`type`.name shouldBe "org.example.Test2"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 22)) if f.getName == "Test2.java" => }
          case "6" =>
            info.name shouldBe "org.example.Test2.compute()"
            info.localName shouldBe "compute"
            info.`type`.name shouldBe "()int"
            info.isCallable shouldBe true
            // NOTE: we should find an OffsetSourcePosition here as the source enters
            // the compiler's working set in case "5" above.
            // TODO - However if the 'element' is not found, we'll fall through to indexer lookup.
            // look into more exhaustive ways of finding the element.
            info.declPos should matchPattern {
              case Some(LineSourcePosition(f, 8)) if f.getName == "Test2.java" =>
              case Some(OffsetSourcePosition(f, 48)) if f.getName == "Test2.java" =>
            }
          case "7" =>
            {}
            info.name shouldBe "org.example.Test1.compute(int,int)"
            info.localName shouldBe "compute"
            info.`type`.name shouldBe "(int,int)int"
            info.isCallable shouldBe true
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 481)) if f.getName == "Test1.java" => }
          case "8" =>
            info.name shouldBe "org.example.Test1.CONST"
            info.localName shouldBe "CONST"
            info.`type`.name shouldBe "int"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 98)) if f.getName == "Test1.java" => }
          case "9" =>
            info.name shouldBe "org.example.Test1.Day"
            info.localName shouldBe "Day"
            info.`type`.name shouldBe "org.example.Test1.Day"
            info.isCallable shouldBe false
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 653)) if f.getName == "Test1.java" => }
          case "10" =>
            info.name shouldBe "org.example.Test1.Day.MON"
            info.localName shouldBe "MON"
            info.`type`.name shouldBe "org.example.Test1.Day"
            info.isCallable shouldBe false
            // Don't specify offset pos here as Java 6 seems to have a problem locating enums
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, i: Int)) if f.getName == "Test1.java" => }
          case "13" | "14" =>
            info.name shouldBe "k"
            info.`type`.name shouldBe "int"
            info.isCallable shouldBe false
        }
      }
  }

  it should "find completions at point" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "import java.lang.Str@5@;",
        "import java.util.Map.E@6@;",
        "import java.util.Map.E@7@blablabla;",
        "class Test1 {",
        "  public static final int MAX_VALUE = 10;",
        "  public static class TestInner {",
        "    public int maxValue = 10;",
        "    private void main(String foo, String bar) {",
        "      File f = new File(\".\");",
        "      f.toSt@0@;",
        "      System.out.println(f.toStr@1@);",
        "      System.out.println((f).toStr@2@);",
        "      System.out.println(f.toString().substr@3@);",
        "      f.@4@;",
        "      new Fi@8@",
        "      System.out.println(fo@9@ + bar);",
        "      System.out.println(maxV@10@);",
        "      System.out.println(MAX_@11@);",
        "      System.out.println(new Inte@12@);",
        "      int testinner = 5;",
        "      System.out.println(f.toStr@1@);",
        "      System.out.@14@",
        "    }",
        "  }",
        "}") { (sf, offset, label, cc) =>
          val info = cc.askCompletionsAtPoint(sf, offset, 0, false)
          label match {
            case "0" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "1" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "2" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "3" => forAtLeast(1, info.completions)(_.name shouldBe "substring")
            case "4" =>
              forAtLeast(1, info.completions)(_.name shouldBe "createTempFile")
              forAtLeast(1, info.completions)(_.name shouldBe "wait")
            case "5" => forAtLeast(1, info.completions)(_.name shouldBe "String")
            case "6" => forAtLeast(1, info.completions)(_.name shouldBe "Entry")
            case "7" => forAtLeast(1, info.completions)(_.name shouldBe "Entry")
            case "8" => forAtLeast(1, info.completions)(_.name shouldBe "File")
            case "9" => forAtLeast(1, info.completions)(_.name shouldBe "foo")
            case "10" => forAtLeast(1, info.completions)(_.name shouldBe "maxValue")
            case "11" => forAtLeast(1, info.completions)(_.name shouldBe "MAX_VALUE")
            case "12" => forAtLeast(1, info.completions)(_.name shouldBe "Integer")

            case "13" =>
              // exact matches should be preferred
              info.completions(0).name shouldBe "TestInner"
              info.completions(1).name shouldBe "testinner"

            case "14" => forAtLeast(1, info.completions)(_.name shouldBe "println")
          }
        }
    }
  }

  it should "find completion at beginning of file" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc, "Sys@0@") { (sf, offset, label, cc) =>
        val info = cc.askCompletionsAtPoint(sf, offset, 0, false)
        label match {
          case "0" => forAtLeast(1, info.completions)(_.name shouldBe "System")
        }
      }
    }
  }

  it should "find doc sig at point" in withJavaCompiler { (_, config, cc, store, search) =>
    runForPositionInCompiledSource(config, cc,
      "import java.io.Fi@5@le;",
      "class Test1 {",
      "  private void main() {",
      "    File f = new F@1@ile(\".\")",
      "    System.out.println(f.toStr@2@ing());",
      "    File.create@3@TempFile(\"bla\", \"foo\");",
      "    File.create@4@TempFile(\"bla\", \"foo\", f);",
      "    System.out.println(\"bla\".ind@6@exOf(\"b\"));",
      "    System.out.println(\"bla\".index@7@Of(\"b\", 1));",
      "    System.out.println(\"bla\".index@8@Of(1));",
      "  }",
      "}") { (sf, offset, label, cc) =>
        val sig = cc.askDocSignatureAtPoint(sf, offset).get.java
        label match {
          case "0" => sig.fqn shouldBe DocFqn("", "Test1")
          case "1" => sig.fqn shouldBe DocFqn("java.io", "File")
          case "2" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("toString()"));
          case "3" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("createTempFile(java.lang.String,java.lang.String)"));
          case "4" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("createTempFile(java.lang.String,java.lang.String,java.io.File)"));
          case "5" => sig.fqn shouldBe DocFqn("java.io", "File")
          case "6" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(java.lang.String)"));
          case "7" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(java.lang.String,int)"));
          case "8" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(int)"));
        }
      }
  }

}
