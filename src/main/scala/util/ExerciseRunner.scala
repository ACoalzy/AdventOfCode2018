package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day13.txt").toList
  val model = Day13.parseInput(lines)
  println(Day13.simulateUntilCrash(model))
  println(Day13.lastCartStanding(model))

}
