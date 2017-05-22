package org.jetbrains.plugins.scala
package annotator
package template

import com.intellij.openapi.extensions.Extensions
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.typeInference.testInjectors.SCL9446Injector

class NeedsToBeAbstractBugsTest extends AnnotatorTestBase(NeedsToBeAbstract)

  def testSCL2981(): Unit =
    assertMatches(messages(
            "trait A { type T; def t(p: T)}; class B extends A { type T = Int; def t(p: T) = ()}"))
      case Nil =>

  def testSCL3515(): Unit =
    assertMatches(messages("trait A { type T}; class B extends A"))
      case Nil =>

  def testSCL3514(): Unit =
    val code = """
trait M[X]
abstract class A {
  def foo[A: M]
  def bar[A](implicit oa: M[A])
}

class B extends A {
  def foo[A](implicit oa: M[A]) = ()
  def bar[A: M] = ()
}
    """
    assertMatches(messages(code))
      case Nil =>

  def testSCL4258(): Unit =
    val code = """
        |abstract class Parent {
        |  def m(p: T forSome {type T})
        |}
        |class Child extends Parent {
        |  def m(p: T forSome {type T}) { }
        |}
      """.stripMargin
    assertMatches(messages(code))
      case Nil =>

  def testSCL9446(): Unit =
    val extensionPoint = Extensions.getRootArea.getExtensionPoint(
        SyntheticMembersInjector.EP_NAME)
    val injector = new SCL9446Injector
    extensionPoint.registerExtension(injector)
    try
      val code = """
          |object ppp {
          |trait A {
          |  def foo(): Int
          |}
          |
          |class B extends A {
          |}
          |}
        """.stripMargin
      assertMatches(messages(code))
        case Nil =>
    finally
      extensionPoint.unregisterExtension(injector)
