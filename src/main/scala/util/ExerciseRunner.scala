package util

import exercises.Day4

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day4.txt").toList
  val entries = Day4.parseInput(lines)
  println(Day4.sleepiestGuardAndMinute(entries))
  println(Day4.sleepiestMinuteAndGuard(entries))
}
