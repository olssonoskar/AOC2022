package olsson.aoc2022

import scala.collection.mutable

/**
 * I did not come up with part 2 on my own
 * Due to properties of modulus, patterns emerge where results will be the same
 * By multiplying the divisor of every monkey, we get an integer N where the evaluations result cycle
 * This means that we can use the remainder of the modulus as a reduced worry level without impacting further evaluations
 *
 * Worry = 0, Test = 2 --> 0 % 2 == 0 | Test = 3 --> 0 % 3 == 0 | Test = 5 --> 0 % 5 == 0 |
 * Worry = 1, Test = 2 --> 1 % 2 == 1 | Test = 3 --> 1 % 3 == 1 | Test = 5 --> 1 % 5 == 1 |
 * Worry = 2, Test = 2 --> 2 % 2 == 0 | Test = 3 --> 2 % 3 == 2 | Test = 5 --> 2 % 5 == 2 |
 * Worry = 3, Test = 2 --> 3 % 2 == 1 | Test = 3 --> 3 % 3 == 0 | Test = 5 --> 3 % 5 == 3 |
 * Worry = 4, Test = 2 --> 4 % 2 == 0 | Test = 3 --> 4 % 3 == 1 | Test = 5 --> 4 % 5 == 4 |
 * Worry = 5, Test = 2 --> 5 % 2 == 1 | Test = 3 --> 5 % 3 == 2 | Test = 5 --> 5 % 5 == 0 |
 * Worry = 6, Test = 2 --> 6 % 2 == 0 | Test = 3 --> 6 % 3 == 0 | Test = 5 --> 6 % 5 == 1 |   (2 and 3 loop here, 2 * 3 = 6)
 * Worry = 30 Test = 2 --> 30 % 2 == 0 | Test = 3 --> 30 % 3 == 0 | Test = 5 --> 30 % 5 == 0 |  (2, 3 and 5 loop here, as 2 * 3 * 5 = 30)
 *
 * Since all the evaluations in the test are primes, GCD of the evaluations should be their product
 */

class Day11 extends InputReader {

  private type Throw = (Long, Int)

  def part1(file: String = "day11.txt"): Long = {
    val monkeys = parse(readLines(file))
    for i <- 0 until 20 do takeTurn(monkeys)
    monkeys.sorted.reverse.take(2).map(_.inspections()).product
  }

  def part2(file: String = "day11.txt"): Long = {
    val monkeys = parse(readLines(file))
    val modProduct = monkeys.map(_.action.divisor).product
    for i <- 0 until 10000 do takeTurn(monkeys, manageStress = modProduct)
    monkeys.sorted.reverse.take(2).map(_.inspections()).product
  }

  private def parse(input: List[String]): Array[Monkey] = {
    input.grouped(7)
      .map(text => {
        val itemQueue = mutable.ArrayDeque[Long]()
        val items = text(1).split(" ").drop(4).map(_.replace(",", "")).map(_.toLong)
        itemQueue ++= items
        val operation = text(2).split(" ").reverse.take(2)
        val action = text.slice(3, 6).map(_.split(" ")).map(_.last.toInt)
        Monkey(itemQueue, (operation(1), operation(0)), ThrowAction(action.head, action(1), action(2)))
      }).toArray
  }

  private def takeTurn(monkeys: Array[Monkey], manageStress: Int = 0): Unit = {
    for i <- monkeys.indices
      do
        monkeys(i).inspectAndThrow(manageStress)
          .foreach((item, thrownTo) => {
            monkeys(thrownTo).itemsHeld.append(item)
          })
  }

  private case class Monkey(
                             itemsHeld: mutable.ArrayDeque[Long],
                             operation: (String, String),
                             action: ThrowAction
                           ) {
    private var inspectionsSum = 0

    def inspectAndThrow(stressManaged: Int): List[Throw] = {
      itemsHeld.removeAll()
        .map(item => {
          inspectionsSum += 1
          val stress = operation match
              case ("+", "old") => item + item
              case ("*", "old") => item * item
              case ("+", number) => item + number.toLong
              case ("*", number) => item * number.toLong
          val actualStress = if stressManaged == 0 then stress / 3 else stress % stressManaged
          (actualStress, action.evaluate(actualStress))
        }).toList
    }

    def inspections(): Long = {
      inspectionsSum
    }
  }

  private implicit val monkeyOrder: Ordering[Monkey] = Ordering.by(_.inspections())

  private case class ThrowAction(
                                  divisor: Int,
                                  first: Int,
                                  other: Int
                                ) {
    def evaluate(stress: Long): Int = {
      if stress % divisor == 0 then first else other
    }
  }
}
