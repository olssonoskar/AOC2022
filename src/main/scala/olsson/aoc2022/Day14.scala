package olsson.aoc2022

import olsson.aoc2022.util.{GridSearch, Point}

import scala.collection.mutable.ArrayBuffer

class Day14 extends InputReader {

  def part1(file: String = "day14.txt"): Int = {
    val rockPaths = readLines(file).map(_.split(" -> ").map(Point.from))
    val caveDimension = setDimension(rockPaths.flatten)
    val grid = Grid.from(caveDimension)
    traceRock(rockPaths, caveDimension, grid)
    var count = 0
    while simulateSand(grid, caveDimension)._2 != Material.Abyss.value
    do count += 1
    count
  }

  def part2(file: String = "day14.txt"): Int = {
    val rockPaths = readLines(file).map(_.split(" -> ").map(Point.from))
    val caveDimension = setDimension(rockPaths.flatten).adjustedPart2
    val grid = Grid.from(caveDimension, bottomless = false)
    traceRock(rockPaths, caveDimension, grid)
    var count = 0
    while simulateSand(grid, caveDimension)._1 != caveDimension.translate(Point(500,0))
    do count += 1
    count + 1
  }

  private def traceRock(paths: List[Array[Point]], dimension: Dimension, grid: Grid): Unit = {
    paths.flatMap(path => path.zip(path.drop(1)))
      .foreach(pointPair => {
        GridSearch(dimension.translate(pointPair._1), dimension.translate(pointPair._2), grid.grid.map(_.toArray))
          .traverse()
          .foreach(point => grid.set(point, Material.Rock.value))
      })
  }

  private def simulateSand(grid: Grid, dimension: Dimension): (Point, Char) = {
    var current = (dimension.translate(Point(500, 0)), Material.Air.value)
    while (current._2 != Material.Sand.value && current._2 != Material.Abyss.value) {
      current = flow(current._1, grid)
    }
    if (current._2 == Material.Sand.value)
      grid.set(current._1, Material.Sand.value)
    current
  }

  private def flow(p: Point, grid: Grid): (Point, Char) = {
    p.beneath
      .map(p => (p, grid.read(p)))
      .find(pointAndType => pointAndType._2 != Material.Sand.value && pointAndType._2 != Material.Rock.value)
      .getOrElse(p, Material.Sand.value)
  }

  private def setDimension(points: List[Point]): Dimension = {
    points.map(p => Dimension(p.x, p.x, p.y, p.y))
      .reduce((first, other) => {
        Dimension(
          Math.min(first.minWidth, other.minWidth),
          Math.max(first.maxWidth, other.maxWidth),
          Math.min(first.minHeight, other.minHeight),
          Math.max(first.maxHeight, other.maxHeight)
        )
      })
      .adjusted
  }

  private case class Dimension(minWidth: Int, maxWidth: Int, minHeight: Int, maxHeight: Int) {
    def translate(p: Point): Point = {
      Point(p.x - minWidth, p.y - minHeight)
    }

    def adjusted: Dimension = Dimension(minWidth - 1, maxWidth + 1, 0, maxHeight + 1)
    def adjustedPart2: Dimension = Dimension(minWidth - (maxHeight), maxWidth + (maxHeight), 0, maxHeight + 2)
  }

  private case class Grid(grid: Seq[ArrayBuffer[Char]]) {
    def read(point: Point): Char = {
      if onGrid(point) then grid(point.y)(point.x)
      else Material.Abyss.value
    }

    def set(point: Point, char: Char): Unit = {
      grid(point.y)(point.x) = char
    }

    private def onGrid(p: Point): Boolean = p.x >= 0 && p.x <= (grid.head.length - 1) && p.y >= 0 && p.y <= (grid.length - 1)
  }

  private object Grid {
    def from(dimension: Dimension, bottomless: Boolean = true): Grid = {
      val rows = List.fill(dimension.maxHeight - dimension.minHeight)(row(dimension))
      if bottomless then Grid(rows) else Grid(rows.drop(1).appended(row(dimension, Material.Rock.value)))
    }

    private def row(dimension: Dimension, rowType: Char = Material.Air.value): ArrayBuffer[Char] =
      ArrayBuffer.fill(dimension.maxWidth - dimension.minWidth)(rowType)
  }

  private enum Material(char: Char) {
    case Sand extends Material('o')
    case Rock extends Material('#')
    case Air extends Material('.')
    case Abyss extends Material('X')

    def value: Char = char
  }

  private object Material {
    def from(char: Char): Material = {
      Material.values.find(_.value == char).getOrElse(throw IllegalArgumentException("No type was found"))
    }
  }

  // Print the stack of Material.Sand.sign
  private def debug(grid: Grid): Unit = {
    for y <- grid.grid.indices do
      println("")
      for x <- grid.grid.head.indices do
        print(grid.read(Point(x, y)))
    println(s"${System.lineSeparator()}")
  }
}
