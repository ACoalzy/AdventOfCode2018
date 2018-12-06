package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day6.txt").toList

  val model = Day6.parseInput(lines)
  println(Timer.time(Day6.maxArea(model)))
  println(Timer.time(Day6.totalManhattanArea(10000, model)))

}
