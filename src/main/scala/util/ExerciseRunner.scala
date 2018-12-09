package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day9.txt").toList
  val rules = Day9.parseInput(lines.head)
  println(Day9.highestScore(rules))
  println(Day9.highestScoreVector(rules))
  println(Day9.highestScore(rules.copy(marbles = rules.marbles * 100)))
}
