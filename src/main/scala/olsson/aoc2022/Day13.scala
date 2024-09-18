package olsson.aoc2022

import scala.collection.mutable.ListBuffer

class Day13 extends InputReader {

  /**
   * ?= Positive LookAhead, Match if followed by group, in this case any of '[' ']' ','
   * ?<= Positive LookBehind, Match if preceded by group, in this case any of '[' ']' ','
   *
   * There is also negative versions of LookAhead and LookBehind ( ?! and ?<!) that match if they are NOT followed/preceded
   * by the specified group
   *
   * The regex will ensure that we split on every bracket and comma and keep them as a list of strings
   * We then drop the comma before parsing them into our new format
   */
  private val LookAroundRegex = "((?=[\\[\\],])|(?<=[\\[\\],]))"

  def part1(file: String = "day13.txt"): Int = {
    parseInput(readLines(file))
      .grouped(2).toList
      .map(grp => evaluate(grp.head, grp.last))
      .zipWithIndex.filter(_._1 == Result.CORRECT)
      .map(_._2 + 1)
      .sum
  }

  def part2(file: String = "day13.txt"): Int = {
    val dividers = parseInput(List("[[2]]", "[[6]]"))
    val sorted = (dividers ::: parseInput(readLines(file)))
      .sortWith((left, right) => evaluate(left, right) == Result.CORRECT)
    sorted.zipWithIndex
      .filter(sorted => dividers.contains(sorted._1))
      .map(_._2 + 1)
      .product
  }

  private def parseInput(in: List[String]): List[Wrapper] = {
    in.filter(_.nonEmpty)
      .map(_.split(LookAroundRegex).filter(_ != ",").iterator)
      .map(parseArrays)
  }

  // Recursively parse lists from input into wrapper objects
  private def parseArrays(in: Iterator[String]): Wrapper = {
    val collected = ListBuffer[Wrapper]()
    while (in.hasNext)
      in.next() match
        case "]" => return ListWrapper(collected.toList)
        case "[" => collected.append(parseArrays(in))
        case num: String => collected.append(IntWrapper(num.toInt))
    ListWrapper(collected.toList)
  }

  private def compareList(left: ListWrapper, right: ListWrapper): Result = {
    val result = left.sub.lazyZip(right.sub)
      .map(evaluate)
      .find(_ != Result.UNDETERMINED)
    result match
      case Some(res) => res
      case None if left.sub.length < right.sub.length => Result.CORRECT
      case None if left.sub.length > right.sub.length => Result.INCORRECT
      case None => Result.UNDETERMINED
  }

  private def compareInt(left: IntWrapper, right: IntWrapper): Result = {
    if left.num < right.num then
      Result.CORRECT
    else if left.num > right.num then
      Result.INCORRECT
    else
      Result.UNDETERMINED
  }

  private def evaluate(left: Wrapper, right: Wrapper): Result = {
    (left, right) match
      case (l: IntWrapper, r: IntWrapper) => compareInt(l, r)
      case (l: ListWrapper, r: ListWrapper) => compareList(l, r)
      case (l: ListWrapper, r: IntWrapper) => compareList(l, ListWrapper(List(r)))
      case (l: IntWrapper, r: ListWrapper) => compareList(ListWrapper(List(l)), r)
      case _ => throw UnsupportedOperationException("Invalid combination of wrapping types")
  }

  private sealed abstract class Wrapper

  private case class IntWrapper(num: Int) extends Wrapper

  private case class ListWrapper(sub: List[Wrapper]) extends Wrapper

  private enum Result {
    case CORRECT, INCORRECT, UNDETERMINED
  }
}
