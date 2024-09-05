package olsson.aoc2022

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day09 extends InputReader {

  private val visited = new mutable.HashSet[Position]()
  private var rope = ArrayBuffer[Position]()

  def part1(file: String = "day09.txt")(implicit tracking: Int = 1): Int = {
    val input = readLines(file)
    rope = ArrayBuffer.fill(tracking + 1)(Position())
    visited.add(rope(tracking))
    moveRope(input)
    visited.count(_ => true)
  }

  def part2(file: String = "day09.txt")(implicit tracking: Int = 9): Int = {
    val input = readLines(file)
    rope = ArrayBuffer.fill(tracking + 1)(Position())
    visited.add(rope(tracking))
    moveRope(input)
    visited.count(_ => true)
  }

  private def moveRope(input: List[String])(implicit tracking: Int): Unit = {
    input.map(_.split(" "))
      .foreach(directionAndSteps =>
        val times: Int = directionAndSteps(1).toInt
        directionAndSteps(0) match
          case "R" => stepHead(List.fill(times)(Position(x = 1)))
          case "L" => stepHead(List.fill(times)(Position(x = -1)))
          case "U" => stepHead(List.fill(times)(Position(y = -1)))
          case "D" => stepHead(List.fill(times)(Position(y = 1)))
          case _ => throw IllegalArgumentException("Not a valid move")
      )
  }

  private def stepHead(changes: List[Position])(implicit tracking: Int): Unit = {
    changes.foreach(step => {
      rope(0) = rope.head.move(step)
      updateTail()
      visited.add(rope(tracking))
    })
  }

  private def updateTail(): Unit = {
    (1 until rope.length).foreach(idx =>
      val parent = rope(idx - 1)
      val current = rope(idx)
      if (!parent.isAdjacent(current)) {
        rope(idx) = current.move(parent.compare(current))
      }
    )
  }

  private case class Position(x: Int = 0, y: Int = 0) {
    def isAdjacent(position: Position): Boolean ={
      Math.abs(x - position.x) <= 1 && Math.abs(y - position.y) <= 1
    }

    def move(change: Position): Position = {
      Position(this.x + change.x, this.y + change.y)
    }

    def move(change: (Int, Int)): Position = {
      Position(this.x + change._1, this.y + change._2)
    }
    
    // We use this to decide how to move child knots
    // It will move in the direction of its parent at a maximum of one step for each axis (since comp returns -1 to 1)
    // By applying this only when knots are not adjacent, we ensure that we follow the 'parent' correctly
    def compare(other: Position): (Int, Int) = {
      (this.x.compare(other.x), this.y.compare(other.y))
    }
  }

  // Print rope layout to cmd line
  private def debugPrint(): Unit = {
    val size = 20
    println("\n\r------------ Step -----------------")
    (-size until size).foreach(y =>
      println("\n\r")
      (-size until size).foreach(x =>
        if (rope.exists(p => p.x == x && p.y == y))
          print("x")
        else
          print(".")
      )
    )
  }

}
