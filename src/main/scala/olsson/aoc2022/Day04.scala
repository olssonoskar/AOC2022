package olsson.aoc2022

class Day04 extends InputReader {

  def part1(file: String = "day04.txt"): Int = {
    readLines(file)
      .flatMap(_.split("[,-]"))
      .map(_.toInt)
      .grouped(4)
      .map(nums => (Range(nums.head, nums(1)), Range(nums(2), nums.last)))
      .filter(hasContainingRange)
      .count(_ => true)
  }

  def part2(file: String = "day04.txt"): Int = {
    readLines(file)
      .flatMap(_.split("[,-]"))
      .map(_.toInt)
      .grouped(4)
      .map(nums => (Range(nums.head, nums(1)), Range(nums(2), nums.last)))
      .filter(hasOverlappingRange)
      .count(_ => true)
  }
  
  private def hasContainingRange(first: Range, other: Range): Boolean =
    if first.from < other.from || first.to > other.to then first.contains(other) else other.contains(first)
  
  private def hasOverlappingRange(first: Range, other: Range): Boolean =
    if first.from < other.from || first.to > other.to then first.containsPartial(other) else other.containsPartial(first)

  case class Range(from: Int, to: Int) {

    def contains(other: Range): Boolean = this.from <= other.from && this.to >= other.to
    def containsPartial(other: Range): Boolean = {
      def within(point: Int) = this.from <= point && this.to >= point
      within(other.from) || within(other.to)
    }
  }

}
