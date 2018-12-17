package exercises

import scala.collection.mutable

object Day17 extends DayN {

  override val num: Int = 17

  case class Point(x: Int, y: Int, infinite: Boolean = false) {
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

  def flow(limit: Int, clay: Set[Point], water: Set[Point], p: Point): (Boolean, Set[Point]) = {
    if (p.y > limit) (true, water)
    else if (clay.contains(p) || water.contains(p)) (false, water)
    else if (isFlowing(p.below, clay, water)) (true, water + p)
    else flow(limit, clay, water + p, p.below) match {
      case b @ (belowInfinite, _) if belowInfinite => b
      case (_, belowWater) =>
        val (leftInfinite, leftWater) = flow(limit, clay, belowWater, p.left)
        val (rightInfinite, rightWater) = flow(limit, clay, leftWater, p.right)
        (leftInfinite || rightInfinite, rightWater)
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

  val lines = """x=495, y=2..7
                |y=7, x=495..501
                |x=501, y=3..7
                |x=498, y=2..4
                |x=506, y=1..2
                |x=498, y=10..13
                |x=504, y=10..13
                |y=13, x=498..504""".stripMargin.split("\n").toList

  val spring = Point(500, 0)
  val clay = parseInput(readFile())
  val water = flow(clay.maxBy(_.y).y, clay, Set(), spring)._2
  println(water.count(_.y >= clay.minBy(_.y).y))
  println(stopSpring(clay, water).size)
  prettyPrint(clay, water)
}
