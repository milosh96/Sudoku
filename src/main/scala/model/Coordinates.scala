package model

class Coordinates(val x: Int, val y: Int) {
  def getDistance(c: Coordinates): Double = {
    val xSqr: Int = (x - c.x) * (x - c.x)
    val ySqr: Int = (y - c.y) * (y - c.y)
    Math.sqrt(xSqr + ySqr)
  }
}
