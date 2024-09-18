package olsson.aoc2022.util

import scala.collection.mutable

/** 
 * A* (A Star) like search in 2D Array.
 */

class GridSearch(start: Point, goal: Point, grid: Seq[Array[Char]]) {

  private val lengthBetween = (p1: Point, p2: Point) => Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) // Sort by 'step' distance
  private implicit val pointOrder: Ordering[Point] = Ordering.by(p => lengthBetween(p, goal)).reverse // Reversed to get the shortest path
  private val explore: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(List(start)) // Nodes to evaluate
  private val pathToNode: mutable.HashMap[Point, List[Point]] = mutable.HashMap.empty // Map containing the shortest path to node in key
  private val candidateLength = () => pathToNode.get(goal).map(_.count(_ => true)).getOrElse(Int.MaxValue)
  pathToNode.put(start, List.empty)

  def traverse(): List[Point] = {
    while (explore.nonEmpty) {
      val current = explore.dequeue()
      val path = pathToNode.getOrElse(current, List.empty)
      if(path.count(_ => true) < candidateLength()) {   // Stop exploration if the current path is longer than our candidate
        current.neighbors
          .filter(onGrid) // Only consider spots in the grid
          .foreach(p => updatePath(p, p :: path))
      }
    }
    start :: pathToNode.getOrElse(goal, List.empty)
  }

  private def onGrid(p: Point): Boolean = p.x >= 0 && p.x <= (grid.head.length - 1) && p.y >= 0 && p.y <= (grid.length - 1)
  private def readGrid(point: Point, setup: Boolean = false): Char = grid(point.y)(point.x)

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

  // Prints the path provided to console
  private def debug(path: List[Point]): Unit = {
    for y <- grid.indices do
      println("")
      for x <- grid.head.indices do
        if path.contains(Point(x, y)) then print(readGrid(Point(x, y))) else print(".")
    println(s"${System.lineSeparator()}")
  }
}
