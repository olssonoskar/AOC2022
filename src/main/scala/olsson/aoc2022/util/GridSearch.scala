package olsson.aoc2022.util

import scala.collection.mutable

// Generic A* like search
// Will be kept as a blueprint for solutions that might require pathfinding but with some additional constraints
// Implemented for day 12
class GridSearch(searchNode: Option[(Int, Int)] = None, grid: Array[Array[Char]]) {

  private val start = lookup('S').head
  private val goal = lookup('E').head
  private val lengthBetween = (p1: Point, p2: Point) => Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) // Sort by 'step' distance
  private implicit val pointOrder: Ordering[Point] = Ordering.by(p => lengthBetween(p, goal)).reverse // Reversed to get the shortest path
  private val explore: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(List(start)) // Nodes to evaluate
  private val pathToNode: mutable.HashMap[Point, List[Point]] = mutable.HashMap.empty // Map containing the shortest path to node in key

  pathToNode.put(start, List.empty)

  def traverse(): List[Point] = {
    while (explore.nonEmpty) {
      val current = explore.dequeue()
      val path = pathToNode.getOrElse(current, List.empty)
      current.neighbors
        .filter(onGrid) // Only consider spots in the grid
        .filter(p => climbable(p, current)) // Only consider climbing 1 level or special nodes such as goal
        .foreach(p => updatePath(p, p :: path))
    }
    pathToNode.getOrElse(goal, List.empty)
  }

  private def onGrid(p: Point): Boolean = p.x >= 0 && p.x <= (grid.head.length - 1) && p.y >= 0 && p.y <= (grid.length - 1)

  private def climbable(p: Point, current: Point): Boolean = {
    val elevation = readGrid(current)
    val nextElevation = readGrid(p)
    nextElevation == 'a' || (nextElevation.toInt <= (elevation + 1))
  }

  private def readGrid(point: Point, setup: Boolean = false): Char = {
    val c = grid(point.y)(point.x)
    c match {
      case 'E' if !setup => 'z'
      case 'S' if !setup => 'a'
      case _ => c
    }
  }

  private def lookup(c: Char): Seq[Point] = {
    for
      y <- grid.indices
      x <- grid.head.indices
      if readGrid(Point(x, y), setup = true) == c
    yield Point(x, y)
  }

  // Update the path if provided path to node is shorter than previous
  // If so, we also need to reevaluate the node based on the new path
  private def updatePath(point: Point, path: List[Point]): Unit = {
    val currentPath = pathToNode.get(point).map(_.count(_ => true)).getOrElse(Int.MaxValue)
    if (path.count(_ => true) < currentPath) {
      pathToNode.put(point, path)
      if (!explore.exists(p => p == point)) {
        explore.enqueue(point)
      }
    }
  }

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

  // Prints the path provided to console
  private def debug(path: List[Point]): Unit = {
    for y <- grid.indices do
      println("")
      for x <- grid.head.indices do
        if path.contains(Point(x, y)) then print(readGrid(Point(x, y))) else print(".")
    println(s"${System.lineSeparator()}")
  }
}
