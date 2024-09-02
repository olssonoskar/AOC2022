package olsson.aoc2022
class Day01 extends InputReader {

  def part1(): String = {
    summary().sorted.max.toString
  }
  
  def part2(): String = {
    summary().sorted.reverse.take(3).sum.toString
  }

  private def summary() = {
    val input = readLines("day01.txt")
    val inventories = List.newBuilder[Int]
    var sum = 0
    for row <- input do {
      if row.isBlank then
        inventories += sum
        sum = 0
      else
        sum += row.toInt
    }
    inventories.result()
  }
}
