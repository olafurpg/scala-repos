package org.example

object Foo extends App {

  class Bar {
    val x = 1
  }

  class Foo extends Bar {
    def testMethod(i: Int, s: String) = {
      i + s.length
    }
  }
  val map = Map[String, Int]()
  val foo = new Foo()
  println("Hello, " + foo.x)
  println(foo.testMethod(7, "seven"))

  val fn: String => Int = str => str.size

  fn("foobar")
}

// for SearchServiceSpec
case class CaseClassWithCamelCaseName()
case class Bloo()
case object Blue
