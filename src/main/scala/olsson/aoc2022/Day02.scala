package olsson.aoc2022

import java.lang.Math

class Day02 extends InputReader {

  def part1(fileName: String = "day02.txt"): String = {
    readLines(fileName)
      .map(scoreRound)
      .sum.toString
  }

  def part2(fileName: String = "day02.txt"): String = {
    readLines(fileName)
      .map(actOnRound)
      .map(scoreRound)
      .sum.toString
  }

  private def scoreRound(round: String): Int = {
    round match
      case "C X" => 7
      case "A Y" => 8
      case "B Z" => 9
      case "A X" | "B Y" | "C Z" => 3 + scoreSign(round.last)
      case _ => scoreSign(round.last)
  }

  private def scoreSign(sign: Char): Int = {
    sign match
      case 'X' => 1
      case 'Y' => 2
      case 'Z' => 3
      case _ => throw new IllegalArgumentException("Not a valid sign")
  }

  // In part 2, X,Y,Z means lose, draw, win, respectively, so we change the player sign based on this input
  private def actOnRound(round: String): String = {
    val opponent = Sign.fromOpponent(round).ordinal
    val changeSign = round.last match
      case 'X' => -1
      case 'Y' => 0
      case 'Z' => 1
      case other => throw new IllegalArgumentException(s"Not a valid action: $other")
    // Math.floorMod returns positive mod, so if negative just add value of modulus operator
    val toPlay = Sign.fromOrdinal(Math.floorMod(opponent + changeSign, Sign.values.length)).player
    s"${round.head} $toPlay"
  }

  private enum Sign(val opponent: Char, val player: Char):
    case Rock extends Sign('A', 'X')
    case Paper extends Sign('B', 'Y')
    case Scissor extends Sign('C', 'Z')

  private object Sign {
    def fromOpponent(round: String): Sign = {
      Sign.values.find(_.opponent == round.head)
        .getOrElse(throw new IllegalArgumentException(s"Unexpected sign ${round.head}"))
    }
  }
}