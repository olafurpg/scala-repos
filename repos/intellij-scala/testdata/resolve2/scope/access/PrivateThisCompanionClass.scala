class Foo
  private[this] def f {}

object Foo
  println(new Foo(). /* line: 2, accessible: false */ f)
