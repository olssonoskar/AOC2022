package olsson.aoc2022

class Day08 extends InputReader {

  private type TreeMap = Array[Array[Int]]

  def part1(file: String = "day08.txt"): Int = {
    val input = readLines(file)
    val map: TreeMap = input.map(row => row.split(""))
      .map(_.map(_.toInt))
      .toArray
    var count = 0
    val out = for {
      y <- map.indices
      x <- map.head.indices
      if visible(map, x, y)
    } yield (x, y)
    out.count(_ => true)
  }

  def part2(file: String = "day08.txt"): Int = {
    val input = readLines(file)
    val map: TreeMap = input.map(row => row.split(""))
      .map(_.map(_.toInt))
      .toArray
    var count = 0
    val out = for {
      y <- map.indices
      x <- map.head.indices
    } yield (x, y, viewFrom(map, x, y))
    out.map(_._3).max
  }

  private def visible(map: TreeMap, x: Int, y: Int): Boolean =
    if onEdge(map, x, y) then true
    else {
      minInRowAndColumn(map, x, y) < map(y)(x)
    }

  private def minInRowAndColumn(map: TreeMap, x: Int, y: Int): Int =
    treeRows(map, x, y)
      .map(checkMax)
      .min

  private def viewFrom(map: TreeMap, x: Int, y: Int): Int = {
    val height = map(y)(x)
    val filtered = treeRows(map, x, y)
      .map(trees => countTrees(trees, height))
    filtered.product
  }

  // We reverse left and top here to parse them in order from the perspective of the (x,y) position (closest first)
  // For part 1 we just care about min/max and the order does not matter so we can reuse it
  private def treeRows(map: TreeMap, x: Int, y: Int): List[Array[Int]] = {
    val rowLeft = map(y).take(x).reverse
    val rowRight = map(y).drop(x + 1)
    val colTop = map.map(row => row(x)).take(y).reverse
    val colBot = map.map(row => row(x)).drop(y + 1)
    List(rowLeft, rowRight, colTop, colBot)
  }

  private def countTrees(trees: Array[Int], height: Int): Int = {
    val treesUntilBlocked = trees.takeWhile(_ < height).count(tree => true)
    if treesUntilBlocked == trees.length
    then treesUntilBlocked else treesUntilBlocked + 1
  }

  private def checkMax(in: Array[Int]): Int = if in.isEmpty then 99 else in.max
  private def onEdge(map: TreeMap, x: Int, y: Int): Boolean =
    x == 0 || x == (map.head.length - 1) || y == 0 || y == map.length - 1

}
