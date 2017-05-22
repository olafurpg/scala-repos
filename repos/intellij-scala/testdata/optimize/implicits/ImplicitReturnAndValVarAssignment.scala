trait Scratch
  case object T
  case object U
  case object V
  case object W
  case object X

  object Implicits1
    implicit def TToInt(t: T.type): Int = 0
    implicit def UToInt(t: U.type): Int = 0
    implicit def VToInt(t: V.type): Int = 0
  object Implicits2
    implicit def WToInt(t: W.type): Int = 0
    implicit def XToInt(t: X.type): Int = 0
  import Implicits1.{TToInt, UToInt, VToInt}
  import Implicits2.{WToInt, XToInt}

  def t: Int =
    return T

  def u: Int =
    U

  val v: Int = V

  var w: Int = W

  (X: Int)
/*trait Scratch {
  case object T
  case object U
  case object V
  case object W
  case object X

  object Implicits1 {
    implicit def TToInt(t: T.type): Int = 0
    implicit def UToInt(t: U.type): Int = 0
    implicit def VToInt(t: V.type): Int = 0
  }
  object Implicits2 {
    implicit def WToInt(t: W.type): Int = 0
    implicit def XToInt(t: X.type): Int = 0
  }
  import Implicits1.{TToInt, UToInt, VToInt}
  import Implicits2.{WToInt, XToInt}

  def t: Int = {
    return T
  }

  def u: Int = {
    U
  }

  val v: Int = V

  var w: Int = W

  (X: Int)
}*/
