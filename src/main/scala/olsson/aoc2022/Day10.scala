package olsson.aoc2022

import scala.collection.mutable.ListBuffer

class Day10 extends InputReader {
  private val newLine = System.getProperty("line.separator")
  private val crtPixelWidth = 40
  private var cycle = 0
  private var xReg = 1
  private val registerHistory: ListBuffer[Int] = new ListBuffer[Int]()
  private val crt = new StringBuilder()

  def part1(file: String = "day10.txt"): Int = {
    val input = readLines(file)
    // Noop single step, add takes two cycles
    input.foreach {
      case "noop" =>
        stepSystem()
      case cmd if cmd.startsWith("addx") =>
        stepSystem()
        stepSystem()
        xReg += cmd.split(" ")(1).toInt
    }
    registerHistory.sum
  }
  
  private def stepSystem(): Unit = {
    if (cycle != 0 && cycle % crtPixelWidth == 0) {
      crt.append(newLine)
    }
    if (cycle % crtPixelWidth) >= xReg - 1 && (cycle % crtPixelWidth) <= xReg + 1
    then crt.append("#") else crt.append(".")
    cycle += 1
    if (cycle == 20 || cycle % crtPixelWidth == 20) {
      registerHistory.append(cycle * xReg)
    }
  }

  def part2(file: String = "day10.txt"): String = {
    part1(file)
    crt.result()
  }

}
