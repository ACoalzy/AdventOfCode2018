package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

  val lines = readFile("day8.txt").toList
  val input = Day8.parseInput(lines.head)
  val root = Day8.parseNode(input, Nil, Nil, Nil, None)
  println(Day8.sumMetadata(List(root.get), 0))
  println(Day8.complicatedSum(List(root.get), 0))
}
