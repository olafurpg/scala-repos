/*initInDeclaration*/
class Test
  abstract class A
    def foo()

  new A()
    def foo()
      /*start*/
      1 /*end*/
/*
/*initInDeclaration*/
class Test {
  abstract class A {
    def foo()
  }

  new A() {
    var i: Int = 1

    def foo() {
      i
    }
  }
}
 */
