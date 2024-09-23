package olsson.aoc2022

import olsson.aoc2022.util.Point

class Day15 extends InputReader {

  // DiamondShape - count from y length where x starts at 1, then up to max, then down to 1 again

  def part1(): String = {
    val length = Point(2, 10).lengthTo(Point(8,7))
    val out = for {
      y <- 7 - length until 7 + length
      x <- xRange(8, y - 7, length)
    } yield (x, y)
    out.foreach(println)
    ""
  }

  def part2(file: String = "day15.txt"): String = {
    ""
  }

  def xRange(x: Int, iteration: Int, length: Int): IndexedSeq[Int] = {
    if iteration == 0 then Array(x)
    else if (iteration * 2 < length){
      (x - iteration) to (x + iteration)
    } else {
      x - (length - iteration) to (x + (length -iteration))
    }
  }

  def diamond(): Unit = {

  }

}