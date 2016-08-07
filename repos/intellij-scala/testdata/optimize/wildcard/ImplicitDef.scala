import scala.language.implicitConversions

class ImplicitDef {
  import Mess.{a, s, foo, AAAA, BBBB}

  val x = new AAAA
  val y = new BBBB
  val z = a + s + foo
}

object Mess {
  val a = 1
  val s = "a"
  def foo = 1

  class AAAA
  class BBBB

  implicit def intToString(i: Int): String = i.toString
}

/*
import scala.language.implicitConversions
class ImplicitDef {
  import Mess.{AAAA, BBBB, a, foo, s}
  val x = new AAAA
  val y = new BBBB
  val z = a + s + foo
}
object Mess {
  val a = 1
  val s = "a"
  def foo = 1
  class AAAA
  class BBBB
  implicit def intToString(i: Int): String = i.toString
}
 */
