package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day12.txt").toList

  val model = Day12.parseInput(lines.head, lines.drop(2))
  println(Day12.runModel(model, 20))
  println(Day12.runModel(model, 50000000000L))

}
