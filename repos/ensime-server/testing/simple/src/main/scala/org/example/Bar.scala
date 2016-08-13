package org.example

object Bar extends App {
  case class Foo(bar: String, baz: Int)
  object Bla {
    val foo: Foo = Foo(
      bar = "Bar",
      baz = 123
    )

    val fooUpd = foo.copy(bar = foo.bar.reverse)
  }
}
