package olsson.aoc2022

import scala.collection.mutable

class Day09 extends InputReader {

  private val visited = new mutable.HashSet[Position]()

  def part1(file: String = "day09.txt"): Int = {
    val input = readLines(file)
    var tail = Position()
    var head = Position()
    visited.add(tail)
    
    def step(changes: List[Position]): Unit = {
      changes.foreach(step => {
        head = head.move(step)
        if (!head.isAdjacent(tail)) {
          tail = head.previous()
          visited.add(tail)
        }
      })
    }
    
    input.map(_.split(" "))
      .foreach( directionAndSteps =>
        val times: Int = directionAndSteps(1).toInt
        directionAndSteps(0) match
          case "R" => step(List.fill(times)(Position(x = 1)))
          case "L" => step(List.fill(times)(Position(x = -1)))
          case "U" => step(List.fill(times)(Position(y = 1)))
          case "D" => step(List.fill(times)(Position(y = -1)))
          case _ => throw IllegalArgumentException("Not a valid move")
      )
    visited.count(_ => true)
  }

  def part2(file: String = "day09.txt"): String = {
    ""
  }

  private case class Position(x: Int = 0, y: Int = 0, prevX: Int = 0, prevY: Int = 0) {
    def isAdjacent(position: Position): Boolean ={
      Math.abs(x - position.x) <= 1 && Math.abs(y - position.y) <= 1
    }
    
    def previous(): Position = {
      Position(prevX, prevY)
    }

    def move(change: Position): Position = {
      Position(this.x + change.x, this.y + change.y, this.x, this.y)
    }
  }

}
