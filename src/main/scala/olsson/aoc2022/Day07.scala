package olsson.aoc2022

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day07 extends InputReader {

  private val MaxMemory = 70_000_000
  private val RequiredMemory = 30_000_000

  private val BaseDir = Dir("/", ArrayBuffer.empty, ArrayBuffer.empty)
  private var CurrentDir: Dir = BaseDir
  private val DirPath: mutable.Stack[Dir] = new mutable.Stack()

  def part1(file: String = "day07.txt"): Long = {
    val input = readLines(file).iterator
    parseFileStructure(input)
    val summaries = summary(BaseDir)
    summaries.filter(_ < 100_000).sum
  }

  def part2(file: String = "day07.txt"): Long = {
    val input = readLines(file).iterator
    parseFileStructure(input)
    val sum = summary(BaseDir)
    val memoryUsed = sum.head
    val overCommited = -MaxMemory + RequiredMemory + memoryUsed
    sum.filter(_ > overCommited).min
  }

  private def parseFileStructure(input: Iterator[String]): Unit = {
    input.next()
    while (input.hasNext) {
      val token = input.next()
      token match
        case cmd: String if cmd.startsWith("$ cd") => cd(cmd)
        case cmd: String if cmd.startsWith("$ ls") => // Noop, we just read input next
        case output: String => output match
          case dir: String if dir.startsWith("dir") =>
            CurrentDir.dirs += Dir.empty(dir.split(" ")(1), CurrentDir)
          case file: String =>
            val sizeAndName = file.split(" ")
            CurrentDir.files += File(sizeAndName(1), sizeAndName.head.toInt)
    }
  }

  private def summary(dir: Dir): List[Long] = {
    val size = dir.size() + childrenSum(dir.dirs)
    val children = dir.dirs.flatMap(summary).toList
    size :: children
  }

  private def childrenSum(dirs: ArrayBuffer[Dir]): Long = {
    dirs.map(dir => {
      if dir.dirs.isEmpty then dir.size()
      else dir.size() + childrenSum(dir.dirs)
    }).sum
  }

  private def cd(cmd: String): Unit = {
    val newPath = cmd.split(" ")(2)
    newPath match
      case ".." => CurrentDir = DirPath.pop()
      case into: String =>
        DirPath.push(CurrentDir)
        CurrentDir = CurrentDir.dirs.find(_.name == into)
          .getOrElse(throw new IllegalArgumentException("No folder was found"))
  }

  private case class Dir(name: String, dirs: ArrayBuffer[Dir], files: ArrayBuffer[File]) {
    def size(): Long = files.map(_.size).sum
  }
  private object Dir {
    def empty(name: String, parent: Dir): Dir = Dir(name, ArrayBuffer.empty, ArrayBuffer.empty)
  }
  private case class File(name: String, size: Long)

}
