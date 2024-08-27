import olsson.aoc2022.{Day02, Day03}
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

}
