import olsson.aoc2022.Day13.Result._
import olsson.aoc2022.{Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13}
import org.scalatest.funsuite.AnyFunSuite

class DayTests extends AnyFunSuite {

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
    assert(new Day06().part2("day6_ex.txt") == 26)
  }

  test("Day 7") {
    assert(new Day07().part1("day7_ex.txt") == 95437)
    assert(new Day07().part2("day7_ex.txt") == 24933642)
  }

  test("Day 8") {
    assert(new Day08().part1("day8_ex.txt") == 21)
    assert(new Day08().part2("day8_ex.txt") == 8)
  }

  test("Day 9") {
    assert(new Day09().part1("day9_ex.txt") == 13)
    assert(new Day09().part2("day9_ex.txt") == 1)
    assert(new Day09().part2("day9_ex_pt2.txt") == 36)
  }

  test("Day 10") {
    assert(new Day10().part1("day10_ex.txt") == 13140)

    val expected =
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin
    val result = new Day10().part2("day10_ex.txt")
    assert(result == expected)
  }

  test("Day 11 Part 1") {
    assert(new Day11().part1("day11_ex.txt") == 10605)
  }

  test("Day 11 Part 2") {
    assert(new Day11().part2("day11_ex.txt") == 2713310158L)
  }

  test("Day 12 Part 1") {
    assert(new Day12().part1("day12_ex.txt") == 31)
  }

  test("Day 12 Part 2") {
    assert(new Day12().part2("day12_ex.txt") == 29)
  }

  test("day 13 Part 1") {
    assert(new Day13().part1("day13_ex.txt") == 13)
  }

  test("day 13 Part 2") {
    assert(new Day13().part2("day13_ex.txt") == 140)
  }

  test("day 13 regex") {
    val regex = "((?=[\\[\\],])|(?<=[\\[\\],]))"
    val test = "[[],[[0,[10,1]],3,[4]]]"
    val result = test.split(regex).filter(_ != ",")
    val expected = Array("[", "[", "]", "[", "[", "0", "[", "10", "1", "]", "]", "3", "[", "4", "]", "]", "]")
    assert(result.mkString("Array(", ", ", ")") == expected.mkString("Array(", ", ", ")"))
  }
}
