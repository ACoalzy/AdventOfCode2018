package exercises

object Day6 {

  case class Model(points: Seq[Point]) {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max

    val allPoints = for (x <- minX to maxX; y <- minY to maxY) yield Point(x, y)

    def onEdge(p: Point): Boolean =
      p.x == minX || p.x == maxX || p.y == minY || p.y == maxY
  }

  case class Point(x: Int, y: Int)

  def parseInput(seq: Seq[String]): Model = {
    val regex = "(\\d+), (\\d+)".r
    Model(seq.map {
      case regex(x, y) => Point(x.toInt, y.toInt)
    }.toList)
  }

  private def manDist(p: Point, p2: Point): Int = {
    Math.abs(p.x - p2.x) + Math.abs(p.y - p2.y)
  }

  def maxArea(model: Model) = {
    def minIfNoEqual(m: Seq[(Point, Int)]): Option[(Point, Int)] = {
      val min = m.minBy(_._2)._2
      val filtered = m.filter(_._2 == min)
      if (filtered.size > 1) None else Some(filtered.head)
    }

    val manhattan = model.allPoints.map(p => p -> model.points.map(p2 => (p2, manDist(p, p2)))).toMap
    manhattan.map(m => m._1 -> minIfNoEqual(m._2))
      .filter(_._2.isDefined)
      .map(m => m._1 -> m._2.get)
      .groupBy(_._2._1)
      .map(m => m._1 -> m._2.keys.toList)
      .filterNot(_._2.exists(model.onEdge))
      .maxBy(_._2.size)._2.size
  }

  def totalManhattanArea(limit: Int, model: Model): Int = {
    val manhattan = model.allPoints.map(p => p -> model.points.map(p2 => (p2, manDist(p, p2)))).toMap
    val totalManhattan = manhattan.map(p => p._1 -> p._2.map(_._2).sum)
    totalManhattan.count(_._2 < limit)
  }

}
