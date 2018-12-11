package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

//  val lines = readFile("day10.txt").toList
  println(Day11.largestNxN(5468, 3, 3))
  println(Day11.largestNxN(5468, 1, 300))

}
