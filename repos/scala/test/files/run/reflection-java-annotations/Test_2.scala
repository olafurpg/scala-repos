object Test extends App {
  import scala.reflect.runtime.universe._
  val sym = typeOf[JavaAnnottee_1].typeSymbol
  sym.info
  sym.annotations.foreach(_.javaArgs)
  println(sym.annotations)
  println("=======")
  sym.annotations.map(_.tree).map(println)
}
