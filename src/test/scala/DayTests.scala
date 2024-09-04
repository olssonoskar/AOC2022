import olsson.aoc2022.{Day02, Day03, Day04, Day05, Day06, Day07, Day08}
import org.scalatest.funsuite.AnyFunSuite

class DayTests extends AnyFunSuite{

  test("Day 2") {
    assert(new Day02().part1("day2_ex.txt") == "15")
    assert(new Day02().part2("day2_ex.txt") == "12")
  }

  test("Day 3") {
    assert(new Day03().part1("day3_ex.txt") == "157")
    assert(new Day03().part2("day3_ex.txt") == "70")
  }

  test("Day 4") {
    assert(new Day04().part1("day4_ex.txt") == 2)
    assert(new Day04().part2("day4_ex.txt") == 4)
  }

  test("Day 5") {
    assert(new Day05().part1("day5_ex.txt") == "CMZ")
    assert(new Day05().part2("day5_ex.txt") == "MCD")
  }

  test("Day 6") {
    assert(new Day06().part1("day6_ex.txt") == 11)
    assert(new Day06().part2("day6_ex.txt") == 0)
  }

  test("Day 7") {
    assert(new Day07().part1("day7_ex.txt") == 95437)
    assert(new Day07().part2("day7_ex.txt") == 24933642)
  }

  test("Day 8") {
    assert(new Day08().part1("day8_ex.txt") == 21)
    assert(new Day08().part2("day8_ex.txt") == 8)
  }

}
