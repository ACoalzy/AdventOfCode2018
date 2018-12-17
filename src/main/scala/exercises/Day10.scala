package exercises

import scala.collection.mutable

object Day10 extends DayN {

  override val num: Int = 10

  case class Point(x: Int, y: Int)

  case class Velocity(vx: Int, vy: Int)

  def parseInput(seq: Seq[String]): Set[(Point, Velocity)] = {
    val regex = """position=< ?(-?\d+),  ?(-?\d+)> velocity=< ?(-?\d+),  ?(-?\d+)>""".r
    seq.map {
      case regex(x, y, vx, vy) => Point(y.toInt, x.toInt) -> Velocity(vy.toInt, vx.toInt)
    }.toSet
  }

  private def hasAdjacent(point: Point, points: Set[Point]): Boolean = {
    val adjacents = for (x <- -1 to 1; y <- -1 to 1) yield Point(point.x + x, point.y + y)
    adjacents.count(points.contains) > 1
  }

  @annotation.tailrec
  def findWord(points: Set[(Point, Velocity)]): Set[Point] = {
    val pointKeys = points.map(_._1)
    if (pointKeys.forall(p => hasAdjacent(p, pointKeys))) {
      pointKeys
    } else {
      findWord(points.map { case (p, v) => Point(p.x + v.vx, p.y + v.vy) -> v })
    }
  }

  @annotation.tailrec
  def timeWordFind(points: Set[(Point, Velocity)], time: Int): Int = {
    val pointKeys = points.map(_._1)
    if (pointKeys.forall(p => hasAdjacent(p, pointKeys))) {
      time
    } else {
      timeWordFind(points.map { case (p, v) => Point(p.x + v.vx, p.y + v.vy) -> v }, time + 1)
    }
  }

  def display(points: Set[Point]) = {
    val minX = points.minBy(_.x).x
    val minY = points.minBy(_.y).y
    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y
    val grid = mutable.ArrayBuffer.fill(1 + maxX - minX, 1 + maxY - minY)(0)
    points.foreach(p => grid(p.x - minX)(p.y - minY) = 1)
    println("Display:")
    println(grid.map(_.map(i => if (i == 0) "." else "#").mkString).mkString("\n"))
    println()
  }

  val points = parseInput(readFile())
  val wordPoints = findWord(points)
  display(wordPoints)
  println(timeWordFind(points, 0))

}
