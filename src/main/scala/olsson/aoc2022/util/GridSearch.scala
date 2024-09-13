package olsson.aoc2022.util

import scala.collection.mutable

// Generic A* like search
// Will be kept as a blueprint for solutions that might require pathfinding but with some additional constraints
// Implemented for day 12
// TODO Currently does not have any checks for updating a shorter path when found
private class GridSearch(startX: Int, startY:Int, endX: Int, endY: Int) {

  private val start = Point(startX, startY)
  private val goal = Point(endX, endY)
  private implicit val pointOrder: Ordering[Point] = Ordering.by(p => lengthBetween(p, goal)).reverse // Reversed to get the shortest path
  private val explorable: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(List(start))
  private val visited: mutable.HashSet[Point] = mutable.HashSet()
  private val pathToNode: mutable.HashMap[Point, List[Point]] = mutable.HashMap.empty
  pathToNode.put(start, List.empty)

  def traverse(): List[Point] = {
    while(explorable.nonEmpty) {
      val current = explorable.dequeue()
      visited.add(current)
      val path = pathToNode.getOrElse(current, List.empty)

      if (current == goal)
        return path

      current.neighbors
        .filter(p => !visited.contains(p))
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

object GridSearch {
  @main
  def tryRun(): Unit = {
    val y = GridSearch(0, 0, 3, 3)
    assert(y.traverse().count(_ => true) == 6)
    val x = GridSearch(-4, 2, 3, 3)
    assert(x.traverse().count(_ => true) == 8)
  }
}
