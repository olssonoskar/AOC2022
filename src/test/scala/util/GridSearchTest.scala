package util

import olsson.aoc2022.util.{GridSearch, Point}
import org.scalatest.funsuite.AnyFunSuite

class GridSearchTest extends AnyFunSuite{

  ignore("GridSearch small") {
    val grid =
      """
        |abbqponm
        |aScryxxl
        |abcszExk
        |acctuvwj
        |abdefghi""".stripMargin
    val test = grid.split(System.lineSeparator()).drop(1).map(_.toCharArray)
    val search = GridSearch(Point(0, 0), Point(0, 0), test)
    assert(search.traverse().count(_ => true) == 29)
  }

  ignore("GridSearch small alternative") {
    val grid =
      """
        |abbqponm
        |adcryxxl
        |abcszExk
        |Sbctuvwj
        |dbdefghi""".stripMargin
    val test = grid.split(System.lineSeparator()).drop(1).map(_.toCharArray)
    val search = GridSearch(Point(0, 0), Point(0, 0), test)
    assert(search.traverse().count(_ => true) == 28)
  }

}
