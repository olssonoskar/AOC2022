package olsson.aoc2022

class Day03 extends InputReader {

  private val UpperCaseStart = 27

  def part1(fileName: String = "day03.txt"): String = {
    val input = readLines(fileName)
    input.map(keepDuplicates)
      .flatMap(_.split(""))
      .map(single => priority(single.head))
      .sum.toString
  }

  def part2(fileName: String = "day03.txt"): String = {
    readLines(fileName)
      .grouped(3)
      .map(group => Group(group.head, group(1), group(2)))
      .map(keepDuplicatesFromGroup)
      .flatMap(_.split(""))
      .map(single => priority(single.head))
      .sum.toString
  }

  private def keepDuplicates(line: String): String = {
    val first = line.substring(0, line.length / 2)
    val second = line.substring(line.length / 2)
    val out = first.filter(second.contains(_)).distinct
    out
  }

  private def keepDuplicatesFromGroup(group: Group): String = {
    group.first.filter(group.second.contains(_))
      .filter(group.third.contains(_)).distinct
  }

  private def priority(char: Char): Int = {
    char match
      case _ if char.toInt >= 'A'.toInt && char.toInt <= 'Z'.toInt => char.toInt - 65 + UpperCaseStart   // Range 27-52
      case _ if char.toInt >= 'a'.toInt && char.toInt <= 'z'.toInt => char.toInt - 96                    // Range 1-26
      case _ => throw new IllegalArgumentException("Out of character range")
  }

  private case class Group(first: String, second: String, third: String)

}

object Day03 {

  def main(args: Array[String]): Unit = {
    println(new Day03().part1())
    println(new Day03().part2())
  }
}