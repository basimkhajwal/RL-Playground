package rlp.util

case class Point2D(x: Double, y: Double) {
  def +(that: Point2D) = Point2D(x + that.x, y + that.y)
  def -(that: Point2D) = Point2D(x - that.x, y - that.y)
  def *(scale: Double) = Point2D(x * scale, y * scale)
  def magnitude(): Double = math.sqrt(x*x + y*y)
  def angle(): Double = {
    val a = math.atan2(y, x)
    if (a < 0) a+2*math.Pi else a
  }
}
object Point2D {
  def fromPolar(theta: Double, m: Double): Point2D = Point2D(m*math.cos(theta), m*math.sin(theta))
}
