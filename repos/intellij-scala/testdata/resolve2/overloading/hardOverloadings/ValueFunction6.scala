object Test
  class C
    def apply(x: String): String = ""
  class A
    val foo: C = new C
    def foo(x: Int): String = ""

  val a = new A

  a. /* line: 6 */ foo("")
  a. /* line: 7 */ foo(3)
