package exercises

object Day6 extends DayN {

  override val num: Int = 6

  sealed trait Coord {
    def x: Int
    def y: Int
  }
  case class MainPoint(x: Int, y: Int) extends Coord
  case class Point(x: Int, y: Int) extends Coord

  case class Model(points: Seq[MainPoint]) {
    private val minX = points.map(_.x).min
    private val maxX = points.map(_.x).max
    private val minY = points.map(_.y).min
    private val maxY = points.map(_.y).max

    val allCoords = for (x <- minX to maxX; y <- minY to maxY) yield Point(x, y)

    def distanceSum(c: Coord): Int = points.map(p => manDist(p, c)).sum

    def onEdge(p: Coord): Boolean =
      p.x == minX || p.x == maxX || p.y == minY || p.y == maxY

    def closest(point: Point): Option[(MainPoint, Point)] = {
      val distances = points.map(p => p -> manDist(point, p))
      val min = distances.minBy(_._2)._2
      val filtered = distances.filter(_._2 == min)
      if (filtered.size != 1) None else Some(filtered.head._1 -> point)
    }
  }

  def parseInput(seq: Seq[String]): Model = {
    val regex = "(\\d+), (\\d+)".r
    Model(seq.map {
      case regex(x, y) => MainPoint(x.toInt, y.toInt)
    }.toList)
  }

  private def manDist(p: Coord, p2: Coord): Int = {
    Math.abs(p.x - p2.x) + Math.abs(p.y - p2.y)
  }

  def maxArea(model: Model) = {
    model.allCoords.flatMap(model.closest)
      .groupBy(_._1).mapValues(_.map(_._2))
      .filterNot(_._2.exists(model.onEdge))
      .map(_._2.size).max
  }

  def totalManhattanArea(limit: Int, model: Model): Int = model.allCoords.map(model.distanceSum).count(_ < limit)

  val lines = readFile()
  val model = Day6.parseInput(lines)
  println(Day6.maxArea(model))
  println(Day6.totalManhattanArea(10000, model))
}
