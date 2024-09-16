package olsson.aoc2022

object Runner {

  @main
  def main(args: String*): Unit = {
    days
      .filter(_.day == 13)
      .foreach(output)
  }

  private type DayResult = String | Int | Long // Scala 3 union

  private def output(execution: Execution): Unit = {
    val part1 = execution.part1()
    val part2 = execution.part2()
    part2 match
      case s: String if s.contains(System.getProperty("line.separator")) =>
        println(s"Day ${execution.day}: $part1 &  part 2 ↓")
        println(s)
      case _ => println(s"Day ${execution.day}: $part1 & $part2")

  }

  private val days: List[Execution] = List(
    Execution(1, () => Day01().part1(), () => Day01().part2()),
    Execution(2, () => Day02().part1(), () => Day02().part2()),
    Execution(3, () => Day03().part1(), () => Day03().part2()),
    Execution(4, () => Day04().part1(), () => Day04().part2()),
    Execution(5, () => Day05().part1(), () => Day05().part2()),
    Execution(6, () => Day06().part1(), () => Day06().part2()),
    Execution(7, () => Day07().part1(), () => Day07().part2()),
    Execution(8, () => Day08().part1(), () => Day08().part2()),
    Execution(9, () => Day09().part1(), () => Day09().part2()),
    Execution(10, () => Day10().part1(), () => Day10().part2()),
    Execution(11, () => Day11().part1(), () => Day11().part2()),
    Execution(12, () => Day12().part1(), () => Day12().part2()),
    Execution(13, () => Day13().part1(), () => Day13().part2()),
  )

  private case class Execution(day: Int, part1: () => DayResult, part2: () => DayResult)

}
