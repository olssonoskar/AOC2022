package olsson.aoc2022

import scala.collection.mutable

class Day12 extends InputReader {

  def part1(file: String = "day12.txt"): Int = {
    val grid = readLines(file).map(_.toCharArray).toArray
    val search = GridSearch(0, 0, 3, 3, grid)
    search.traverse()
    0
  }

  private class GridSearch(startX: Int, startY: Int, endX: Int, endY: Int, grid: Array[Array[Char]]) {

    private val start = Point(startX, startY)
    private val goal = Point(endX, endY)
    private implicit val pointOrder: Ordering[Point] = Ordering.by(p => lengthBetween(p, goal)).reverse // Reversed to get the shortest path
    private val explorable: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(List(start))
    private val visited: mutable.HashSet[Point] = mutable.HashSet()
    private val pathToNode: mutable.HashMap[Point, List[Point]] = mutable.HashMap.empty
    pathToNode.put(start, List.empty)

    def traverse(): List[Point] = {
      while (explorable.nonEmpty) {
        val current = explorable.dequeue()
        visited.add(current)
        val path = pathToNode.getOrElse(current, List.empty)
        val elevation = read(current).toInt

        if (current == goal)
          return path

        current.neighbors
          .filter(onGrid)                         // Only consider spots in the grid
          .filter(p => read(p) <= elevation + 1)  // Only consider climbing 1 level
          .filter(p => !visited.contains(p))      // Ignore places we have been
          .foreach(p => {
            pathToNode.put(p, p :: path)
            explorable.enqueue(p)
          })
      }
      List.empty
    }

    // Sort by 'step' distance
    private def lengthBetween(p1: Point, p2: Point): Int = {
      val result = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)
      result
    }

    private def onGrid(p: Point): Boolean = p.x >= 0 && p.x <= grid.head.length - 1 && p.y >= 0 && p.y <= grid.length

    private def read(point: Point): Char = grid(point.y)(point.x)

    case class Point(x: Int, y: Int) {

      def neighbors: List[Point] = {
        List(
          Point(x + 1, y),
          Point(x - 1, y),
          Point(x, y + 1),
          Point(x, y - 1),
        )
      }
    }
  }

}
