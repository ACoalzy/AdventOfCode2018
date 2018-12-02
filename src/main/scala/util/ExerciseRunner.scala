package util

import exercises.Day2

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day2.txt").toList
  println(Day2.getChecksum(lines))
  val pair = Day2.findBoxPair(lines)
  println(pair.map(p => Day2.commonLetters(p._1, p._2)).get.mkString(""))
}
