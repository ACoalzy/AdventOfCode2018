package exercises

object Day25 extends DayN {

  override val num: Int = 25

  case class Point(x: Int, y: Int, z: Int, t: Int) {
    def manDist(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y) + Math.abs(z - p.z) + Math.abs(t - p.t)
  }

  def parseInput(lines: List[String]): Set[Point] = {
    val regex = """(-?\d+),(-?\d+),(-?\d+),(-?\d+)""".r
    lines.map {
      case regex(x, y, z, t) => Point(x.toInt, y.toInt, z.toInt, t.toInt)
    }.toSet
  }

  def countConstellations(points: Set[Point]): Int = {
    @annotation.tailrec
    def go(remaining: Set[Point], acc: List[Set[Point]]): List[Set[Point]] = {
      if (remaining.isEmpty) acc
      else remaining.find(p => acc.head.exists(_.manDist(p) <= 3)) match {
        case Some(p) => go(remaining - p, (acc.head + p) :: acc.tail)
        case None => go(remaining.tail, Set(remaining.head) :: acc)
      }
    }

    go(points.tail, List(Set(points.head))).size
  }

  val points = parseInput(readFile())
  println(countConstellations(points))

}
