package util

import exercises.Day1

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day1.txt").toList
  println(Day1.processFrequencies(lines))
  println(Day1.firstDuplicateFrequency(lines))
}
