package util

import olsson.aoc2022.util.GridSearch
import org.scalatest.funsuite.AnyFunSuite

class GridSearchTest extends AnyFunSuite{

  test("GridSearch small") {
    val grid =
      """
        |abbqponm
        |aScryxxl
        |abcszExk
        |acctuvwj
        |abdefghi""".stripMargin
    val test = grid.split(System.lineSeparator()).drop(1).map(_.toCharArray)
    val search = GridSearch(grid = test)
    assert(search.traverse().count(_ => true) == 29)
  }

  test("GridSearch small alternative") {
    val grid =
      """
        |abbqponm
        |adcryxxl
        |abcszExk
        |Sbctuvwj
        |dbdefghi""".stripMargin
    val test = grid.split(System.lineSeparator()).drop(1).map(_.toCharArray)
    val search = GridSearch(grid = test)
    assert(search.traverse().count(_ => true) == 28)
  }

}
