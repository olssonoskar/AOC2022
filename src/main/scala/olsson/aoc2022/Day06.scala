package olsson.aoc2022

class Day06 extends InputReader {

  private val MakerLength = 4
  private val MessageLength = 14

  def part1(file: String = "day06.txt"): Int = {
    val input = readLines(file)
    val res = distinctWindow(input.head, MakerLength)
    input.head.indexOf(res) + MakerLength
  }

  def part2(file: String = "day06.txt"): Int = {
    val input = readLines(file)
    val res = distinctWindow(input.head, MessageLength)
    input.head.indexOf(res) + MessageLength
  }

  private def distinctWindow(input: String, patternLength: Int): String = {
    (0 until input.length - patternLength)
      .map(i => input.substring(i, i + patternLength))
      .map(_.distinct)
      .find(it => it.length == patternLength)
      .getOrElse("None")
  }
}
