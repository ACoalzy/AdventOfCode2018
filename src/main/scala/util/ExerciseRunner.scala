package util

import exercises._

import scala.io.Source

object ExerciseRunner extends App {

  def readFile(src: String) = {
    Source.fromResource(src).getLines
  }

//  val lines = readFile("day13.txt").toList
  println(Day14.recipesAfterX(3, 7, 110201, 10).mkString)
  println(Day14.countPriorRecipes(3, 7, "110201"))

}
