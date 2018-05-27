final class Natural
    extends scala.math.ScalaNumber
    with scala.math.ScalaNumericConversions {
  def intValue(): Int = 0
  def longValue(): Long = 0L
  def floatValue(): Float = 0.0f
  def doubleValue(): Double = 0.0d
  def isWhole(): Boolean = false
  def underlying() = this
}
