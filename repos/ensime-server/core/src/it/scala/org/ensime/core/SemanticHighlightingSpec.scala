// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.fixture._
import org.ensime.api._
import org.ensime.util.EnsimeSpec

import scala.reflect.internal.util.RangePosition

class SemanticHighlightingSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  def original = EnsimeConfigFixture.EmptyTestProject

  def getSymbolDesignations(
    config: EnsimeConfig,
    cc: RichCompilerControl,
    content: String,
    tpes: List[SourceSymbol] = SourceSymbol.allSymbols
  ): List[(SourceSymbol, String)] = {

    val file = srcFile(config, "abc.scala", contents(content))
    cc.askLoadedTyped(file)
    val pos = new RangePosition(file, 0, 0, file.length)
    val sds = cc.askSymbolDesignationsInRegion(pos, tpes)
    sds.syms.sortWith((a, b) => a.start < b.start).
      map { sym =>
        (sym.symType, content.substring(sym.start, sym.end))
      }
  }

  "SemanticHighlighting" should "highlight classes" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class X1[+A] { }
            class X2 { class Y { } }
            class Test {
              val a: Int = 1
              val b: X1[Any] = new X1[String]()
              val c: X2 = new X2
              val d = new c.Y
              def fun(a: Any) = a match { case x: Test => Unit }
            }
          """,
      List(ClassSymbol)
    )
    sds should ===(List(
      (ClassSymbol, "Int"),
      (ClassSymbol, "X1[Any]"),
      (ClassSymbol, "X1[String]"),
      (ClassSymbol, "X2"),
      (ClassSymbol, "X2"),
      (ClassSymbol, "Y"),
      (ClassSymbol, "Any"),
      (ClassSymbol, "Test")
    ))
  }

  it should "highlight constructors" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class X1(a: Int = 0) { }
            class X2(a: String) { }
            class X3 { }
            class Test {
              val b = new X1(3)
              val c1 = new X1(  )
              val d1 = new X2("y")
              val e1 = new   X3
            }
          """,
      List(ConstructorSymbol)
    )
    // TODO It would be better if the "new" was consistent.
    sds should ===(List(
      (ConstructorSymbol, "X1"),
      (ConstructorSymbol, "new X1(  )"),
      (ConstructorSymbol, "X2"),
      (ConstructorSymbol, "new   X3")
    ))
  }

  it should "highlight function calls" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int) { u + v }
              def foo(u: Int, v: Int) { u + v }
              def bar { val X = fun(1, 2) ; foo(4, 5) }
              val x = "abc".substring(1,2)
              def baz { def quux(): Int = { 1 } ; quux() }
            }
          """,
      List(FunctionCallSymbol)
    )
    sds should ===(List(
      (FunctionCallSymbol, "fun"),
      (FunctionCallSymbol, "foo"),
      (FunctionCallSymbol, "substring"),
      (FunctionCallSymbol, "quux")
    ))
  }

  it should "highlight deprecated symbols" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              @deprecated("bad trait", "1.0")
              trait BadTrait
              class BadSubclass extends BadTrait
              @deprecated("bad class", "1.0")
              class BadClass(val data: String)
              @deprecated("bad object", "1.0")
              object BadObject
              @deprecated("bad method", "1.0")
              def foo(x: String) = { x + " foo" }
              @deprecated("bad val", "1.0")
              val badVal = 42
              @deprecated("bad var", "1.0")
              var badVar = 42
              val a = new BadClass("someData")
              val b = BadObject
              val c = foo("a")
              val d = badVal
              val e = badVar
            }
            @deprecated("blah", "sometime")
            class Baz { def boo() = {} }
            class Foo extends Baz {
              def bar(): Unit = {}
            }
            @deprecated("bad", "someday")
            object X {
              new Foo().bar()
              new Foo().boo()
              object Y { def goo(): Unit = {} }
            }
            object Z {
              X.Y.goo()
            }
                  """,
      List(DeprecatedSymbol)
    )
    sds should ===(List(
      (DeprecatedSymbol, "BadTrait"),
      (DeprecatedSymbol, "BadClass"),
      (DeprecatedSymbol, "BadObject"),
      (DeprecatedSymbol, "foo"),
      (DeprecatedSymbol, "badVal"),
      (DeprecatedSymbol, "badVar"),
      (DeprecatedSymbol, "Baz"),
      (DeprecatedSymbol, "boo"),
      (DeprecatedSymbol, "X"),
      (DeprecatedSymbol, "Y"),
      (DeprecatedSymbol, "goo")
    ))
  }

  it should "support custom deprecated symbol names" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            import scala.annotation.StaticAnnotation
            class deprecating(val p:String) extends StaticAnnotation {}
            class Test {
              @deprecating("AAA")
              trait BadTrait
              class BadSubclass extends BadTrait
            }
          """,
      List(DeprecatedSymbol)
    )

    sds should ===(List(
      (DeprecatedSymbol, "BadTrait")
    ))
  }

  it should "highlight imported names" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            import scala.reflect.internal.util.RangePosition
            import org.scalatest. { Matchers,
                 FunSpec }
            """,
      List(ImportedNameSymbol)
    )
    sds should ===(List(
      (ImportedNameSymbol, "RangePosition"),
      (ImportedNameSymbol, "Matchers"),
      (ImportedNameSymbol, "FunSpec")
    ))
  }

  it should "highlight objects" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            object A { object B { } }
            case class C() { }
            case class D(i: Int) { case class E(i: Int) { } }
            object Test {
              def fun (x:Any) = x match { case C() => 1 }
              val b = A.B
              val c = D(1)
              val d = c.E(1)
            }
          """,
      List(ObjectSymbol)
    )
    sds should ===(List(
      (ObjectSymbol, "C"),
      (ObjectSymbol, "A"),
      (ObjectSymbol, "B"),
      (ObjectSymbol, "D"),
      // TODO two problems there: "c" should be a varField ; E should be highlighted.
      (ObjectSymbol, "c")
    ))
  }

  it should "highlight operators" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            object Test {
              var a = 1 + 2 * 3
              a += 8
            }
          """,
      List(OperatorFieldSymbol)
    )
    // TODO We should highlight the "+="
    sds should ===(List(
      (OperatorFieldSymbol, "+"),
      (OperatorFieldSymbol, "*")
    ))
  }

  it should "highlight packages" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
             package com.example
             package other
          """,
      List(PackageSymbol)
    )
    sds should ===(List(
      (PackageSymbol, "com"),
      (PackageSymbol, "example"),
      (PackageSymbol, "other")
    ))
  }

  it should "highlight params" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def f(u:  Int, v   :String) = v + u
            }
          """,
      List(ParamSymbol)
    )
    sds should ===(List(
      (ParamSymbol, "u"),
      (ParamSymbol, "v"),
      (ParamSymbol, "v"),
      (ParamSymbol, "u")
    ))
  }

  it should "highlight traits" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            trait X1 { }
            trait X2 { }
            trait X3 { }
            trait X4 { }
            trait X5[A] { }
            class C extends X2 { trait X6 {}   }
            class Test {
              def fun(x2: X3) = x2
              var v1: X4
              val v2 = new C
              def foo(x: Any) = x match {
                case _: X5[ String] => x
                case _: v2 .X6 => x
              }
            }
          """,
      List(TraitSymbol)
    )
    sds should ===(List(
      (TraitSymbol, "X2"),
      (TraitSymbol, "X3"),
      (TraitSymbol, "X4"),
      (TraitSymbol, "X5[ String]"),
      (TraitSymbol, "v2 .X6")
    ))
  }

  it should "highlight typeParams" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def f[XX,YY](y: YY, x: XX) = {
                var z: YY
              }
              f[Int, String](1, "a")
            }
          """,
      List(TypeParamSymbol)
    )
    sds should ===(List(
      (TypeParamSymbol, "XX"),
      (TypeParamSymbol, "YY"),
      (TypeParamSymbol, "YY"),
      (TypeParamSymbol, "XX"),
      (TypeParamSymbol, "YY")
    ))
  }

  it should "highlight vals" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def fun() = {
                val u = 1
                val v: Int = 1
                println(u)
              }
            }
          """,
      List(ValSymbol)
    )
    sds should ===(List(
      (ValSymbol, "u"),
      (ValSymbol, "v"),
      (ValSymbol, "u")
    ))
  }

  it should "highlight valFields" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              val u= 1
              val v:Int = 1
              println( u )
            }
            class Test2 {
              println((new Test).v)
            }
          """,
      List(ValFieldSymbol)
    )
    sds should ===(List(
      (ValFieldSymbol, "u"),
      (ValFieldSymbol, "v"),
      (ValFieldSymbol, "u"),
      (ValFieldSymbol, "v")
    ))
  }

  it should "highlight vars" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def fun() = {
                var u = 1
                var v: Int = 1
                println(u)
              }
            }
          """,
      List(VarSymbol)
    )
    sds should ===(List(
      (VarSymbol, "u"),
      (VarSymbol, "v"),
      (VarSymbol, "u")
    ))
  }

  it should "highlight varFields" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              var u= 1
              var v:Int   = 1
              println( u )
            }
            class Test2 {
              println((new Test).v)
            }
          """,
      List(VarFieldSymbol)
    )
    sds should ===(List(
      (VarFieldSymbol, "u"),
      (VarFieldSymbol, "v"),
      (VarFieldSymbol, "u"),
      (VarFieldSymbol, "v")
    ))
  }

  it should "highlight lazy val fields correctly as vals" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              lazy val u= 1
              lazy val v:Int   = 1
              println( u )
            }
            class Test2 {
              println((new Test).v)
            }
          """,
      List(ValFieldSymbol)
    )
    sds should ===(List(
      (ValFieldSymbol, "u"),
      (ValFieldSymbol, "v")
    ))
  }

  it should "highlight setter operators" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            object Fubar {
               private var v: Int = 0
               def value = v
               def value_=(a: Int) = v = a
            }
            class Test {
              Fubar.value = 1
            }
          """,
      List(OperatorFieldSymbol)
    )
    sds should ===(List(
      (OperatorFieldSymbol, "value")
    ))
  }

  it should "not be confused by whitespace" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.  example
          """,
      List(PackageSymbol)
    )
    // only part of "example" is highlighted
    sds should ===(List(
      (PackageSymbol, "com"),
      (PackageSymbol, "example")
    ))
  }

  it should "highlight negation operators" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              val x = !(3 == 4)
            }
          """,
      List(OperatorFieldSymbol)
    )
    // Call to foo is missing
    sds should ===(List(
      (OperatorFieldSymbol, "!"),
      (OperatorFieldSymbol, "==")
    ))
  }

  it should "highlight implicit conversions" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {}
            object I {
              implicit def StringToTest(v: String): Test = new Test
              val t: Test  = "sample";
              val u: Test  = StringToTest("y");
            }
          """,
      List(ImplicitConversionSymbol)
    )
    sds should ===(List(
      (ImplicitConversionSymbol, "\"sample\"")
    ))
  }

  it should "highlight implicit parameters" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Thing {}
            class Thong {}
            object I {
              implicit def myThing = new Thing
              implicit val myThong = new Thong
              def zz(u: Int)(implicit s: Thing, t: Thong) = u
              val t = zz(1)
            }
          """,
      List(ImplicitParamsSymbol)
    )
    sds should ===(List(
      (ImplicitParamsSymbol, "zz(1)")
    ))
  }

  it should "highlight method calls after operators" in withPresCompiler { (config, cc) =>
    val sds = getSymbolDesignations(
      config, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int): Int = { u + v }
              def foo(u: Int, v: Int): Int = { u - v }
              fun(1, 2) + foo(4, 5)
            }
          """,
      List(FunctionCallSymbol)
    )
    sds should ===(List(
      (FunctionCallSymbol, "fun"),
      (FunctionCallSymbol, "foo")
    ))
  }
}
