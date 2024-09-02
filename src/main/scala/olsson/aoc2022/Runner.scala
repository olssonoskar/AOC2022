package olsson.aoc2022

object Runner {

  @main
  def main(args: String*): Unit = {
    output(1, Day01().part1(), Day01().part2())
    output(2, Day02().part1(), Day02().part2())
    output(3, Day03().part1(), Day03().part2())
  }

  private def output(day: Int, part1: String, part2: String): Unit = {
    Console.println(s"Day $day: $part1 & $part2")
  }

}
