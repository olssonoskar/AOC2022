package olsson.aoc2022.util

case class Point(x: Int, y: Int) {
  def neighbors: List[Point] = {
    List(
      Point(x + 1, y),
      Point(x - 1, y),
      Point(x, y + 1),
      Point(x, y - 1),
    )
}

  def beneath: List[Point] = {
    List(
      Point(x, y + 1),
      Point(x - 1, y + 1),
      Point(x + 1, y + 1),
    )
  }

  def lengthTo(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
}
object Point {
  def from(point: (Int, Int)): Point = {
    Point(point._1, point._2)
  }
  def from(input: String): Point = {
    val parts = input.split(",")
    Point(parts.head.toInt, parts.last.toInt)
  }
}
