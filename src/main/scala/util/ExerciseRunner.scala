package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day7.txt").toList

  val nodes = Day7.parseInput(lines)
  println(Day7.navigateTree(nodes))
  println(Day7.navigateTreeWithHelpersTime(nodes, 5, 60))

}
