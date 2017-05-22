package test

trait Vector[+A]
  def append(x: Vector[A]): Vector[A]
  private[this] def append3(x: Vector[A]): Vector[A] = append(x)

class C[T]

object Covariant
  class Foo[+A]
    private[this] var a: A = _
    def getA: A = a
    // allowed
    private[this] def setA1(a: A) = this.a = a
    protected[this] def setA2(a: A) = this.a = a
    // forbidden
    private def setA3(a: A) = this.a = a
    protected def setA4(a: A) = this.a = a

    object Baz extends C[A]
    trait Convert[B]
      def b2a(b: B): A
      def doit1(b: B) = setA1(b2a(b))
      def doit2(b: B) = setA2(b2a(b))
      def doit3(b: B) = setA3(b2a(b))
      def doit4(b: B) = setA4(b2a(b))
  class Foo2[+A]
    private[this] var a: A = _
    def getA: A = a
    private[this] def setA(a: A) = this.a = a

      trait Convert[B]
        def b2a(b: B): A
        def doit(b: B) = setA(b2a(b))
      println("")
  class Foo3[+A]
    private[this] var a: A = _
    def getA: A = a
    private[this] def setA(a: A) = this.a = a

    private[this] trait Convert[B]
      def b2a(b: B): A
      def doit(b: B) = setA(b2a(b))
  abstract class AbstractTest
    val a: Foo[AnyRef]
    val c = new a.Convert[Int]
      def b2a(b: Int): AnyRef = "hello"
    val b: Int = 42
  class Test extends AbstractTest
    val a: Foo[java.lang.Character] = new Foo[java.lang.Character]
  def main(args: Array[String])
    val test = new Test
    test.c.doit1(test.b)
    test.c.doit2(test.b)
    test.c.doit3(test.b)
    test.c.doit4(test.b)
    val x: java.lang.Character = test.a.getA
    Console.println("XXX " + x)

  abstract class T[+A]
    val x: T[A]
      val m: A => A
  object ST extends T[String]
    val x: T[String] { val m: String => String } = ST
    val m: String => String = (_.substring(1))
  val t: T[Any] = ST
  t.x.m(new Object)

object TestAlias
  class B[-T]
  trait C[+T]
    type A = T
    def foo: B[A]
