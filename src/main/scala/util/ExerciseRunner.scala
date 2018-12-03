package util

import exercises.Day3

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day3.txt").toList
  val claims = Day3.parseInput(lines)
  println(Day3.inchesOfOverlap(claims))
  println(Day3.findNoOverlapSquare(claims))
}
