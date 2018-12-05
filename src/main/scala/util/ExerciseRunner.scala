package util

import exercises.Day5

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day5.txt").toList
  println(Timer.time(Day5.compress(lines(0))))
  println(Timer.time(Day5.superCompress(lines(0))))
}
