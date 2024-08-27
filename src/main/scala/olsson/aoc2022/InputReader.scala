package olsson.aoc2022

import scala.io.Source

trait InputReader {
  
  protected def readLines(fileName: String): List[String] = {
    val source = Source.fromResource(fileName) 
    val input = source.getLines().toList
    source.close()
    input
  }

}
