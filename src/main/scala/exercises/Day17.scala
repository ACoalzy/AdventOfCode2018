package exercises

import scala.collection.mutable

object Day17 extends DayN {

  override val num: Int = 17

  case class Point(x: Int, y: Int) {
    def above = Point(x, y - 1)
    def below = Point(x, y + 1)
    def left = Point(x - 1, y)
    def right = Point(x + 1, y)
  }

  def parseInput(lines: List[String]): Set[Point] = lines.flatMap(s => {
      val parts = s.split(", ")
      val part = parts.head.drop(2).toInt
      val bits = parts(1).drop(2).split("\\.\\.")
      val range = bits.head.toInt to bits(1).toInt
      if (parts.head.startsWith("x")) for (y <- range) yield Point(part, y)
      else for (x <- range) yield Point(x, part)
    }).toSet

  private def isFlowing(p: Point, clay: Set[Point], water: Set[Point]): Boolean = {
    @annotation.tailrec
    def isInfinite(q: Point, f: Point => Point): Boolean =
      if (water.contains(f(q))) isInfinite(f(q), f) else !clay.contains(f(q))
    water.contains(p) && (isInfinite(p, _.left) || isInfinite(p, _.right))
  }

  @annotation.tailrec
  def flow(limit: Int, clay: Set[Point], water: Set[Point], acc: List[Point]): Set[Point] = {
    def blocked(p: Point) = clay.contains(p) || water.contains(p)
    acc match {
      case h :: t =>
        if (h.y > limit) flow(limit, clay, water, t)
        else if (isFlowing(h.below, clay, water)) flow(limit, clay, water + h, t)
        else if (blocked(h.below)) {
          val tail = if (water.contains(h.above)) h.above :: t else t
          flow(limit, clay, water + h, List(h.left, h.right).filterNot(blocked) ::: tail)
        }
        else flow(limit, clay, water + h, h.below :: t)
      case Nil => water
    }
  }

  def stopSpring(clay: Set[Point], water: Set[Point]): Set[Point] = water.filterNot(p => isFlowing(p, clay, water))

  def prettyPrint(clay: Set[Point], water: Set[Point]): Unit = {
    val minX = Math.min(clay.minBy(_.x).x, water.minBy(_.x).x)
    val maxX = Math.max(clay.maxBy(_.x).x, water.maxBy(_.x).x)
    val maxY = Math.max(clay.maxBy(_.y).y, water.maxBy(_.y).y)

    val grid = mutable.ArrayBuffer.fill(maxY + 4, 1 + maxX - minX)(".")
    clay.foreach(p => grid(p.y)(p.x - minX) = "#")
    water.foreach(p => grid(p.y)(p.x - minX) = "~")
    println(grid.map(_.mkString).mkString("\n"))
    println()
  }

  val spring = Point(500, 0)
  val clay = parseInput(readFile())
  val water = flow(clay.maxBy(_.y).y, clay, Set(), List(spring))
  val minY = clay.minBy(_.y).y
  prettyPrint(clay, water)
  println(water.count(_.y >= minY))
  println(stopSpring(clay, water).size)

}
