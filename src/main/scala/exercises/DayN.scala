package exercises

import scala.io.Source

trait DayN extends App {

  val num: Int

  def readFile() = Source.fromResource(s"day$num.txt").getLines.toList

}
