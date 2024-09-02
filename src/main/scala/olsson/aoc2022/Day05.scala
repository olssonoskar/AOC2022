package olsson.aoc2022

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day05 extends InputReader {

  private type Piles = Array[mutable.Stack[Char]]
  private val ColumnWidthWithSpace = 4

  def part1(file: String = "day05.txt"): String = {
    val input = readLines(file)
    runWithMode(input, singleMove)
  }

  def part2(file: String = "day05.txt"): String = {
    val input = readLines(file)
    runWithMode(input, moveOrderIntact)
  }

  private def runWithMode(input: List[String], executor: (List[Move], Piles) => Unit): String = {
    val piles = setup(input)
    val moves = input.dropWhile(!_.contains("move"))
      .flatMap(_.split(" "))
      .filter(_.head.isDigit)
      .map(_.toInt)
      .grouped(3)
      .map(nums => Move(nums.head, nums(1), nums(2)))
      .toList
    executor(moves, piles)
    result(piles)
  }

  private def singleMove(moves: List[Move], piles: Piles): Unit = {
    moves.foreach(move =>
      (0 until move.amount)
        .foreach(_ => piles(move.to - 1).push(piles(move.from - 1).pop()))
    )
  }

  private def moveOrderIntact(moves: List[Move], piles: Piles): Unit = {
    moves.map(move =>
      val order = new StringBuilder()
      (0 until move.amount).foreach(_ =>
        order.addOne(piles(move.from - 1).pop())
      )
      piles(move.to - 1).pushAll(order.reverse.result())
    )
  }

  private def setup(input: List[String]): Array[mutable.Stack[Char]] = {
    val setup = input.takeWhile(_.nonEmpty)
    val numPiles = setup.last.split(" +").last.toInt
    // Eat the extra space so we do not get leftover spaces that result in more matches
    val nonEmpty = setup.init.map(_.replaceAll(s" {$ColumnWidthWithSpace}","[ ]"))
    val piles = pileArray(numPiles)
    nonEmpty.reverse
      .foreach(state => {
        var stack = 0
        state.foreach {
          case ']' => stack += 1
          case c: Char if c != '[' && c != ' ' => piles(stack).push(c)
          case _ =>
        }
      }
    )
    piles
  }

  private def pileArray(n: Int): Array[mutable.Stack[Char]] = {
    val out = ArrayBuffer.empty[mutable.Stack[Char]]
    (0 until n).foreach(num => out.addOne(new mutable.Stack[Char]()))
    out.toArray
  }

  private def result(piles: Piles): String = {
    val stringBuilder = new StringBuilder()
    piles.foreach(pile => stringBuilder.addOne(pile.pop()))
    stringBuilder.result()
  }

  private case class Move(amount: Int, from: Int, to: Int)

}
