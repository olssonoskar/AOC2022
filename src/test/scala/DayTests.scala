import olsson.aoc2022.{Day02, Day03, Day04, Day05}
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
    //assert(new Day05().part1("day5_ex.txt") == "CMZ")
    assert(new Day05().part2("day5_ex.txt") == "MCD")
  }

}
