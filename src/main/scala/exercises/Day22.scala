package exercises

import util.Timer

import scala.collection.mutable

object Day22 extends DayN {

  override val num: Int = 22

  case class Point(x: Int, y: Int) {
    def adjacents: Set[Point] = Set(Point(x + 1, y), Point(x - 1, y), Point(x, y + 1), Point(x, y - 1)).filter(_.x >= 0).filter(_.y >= 0)
  }
  case class Model(depth: Int, target: Point)

  sealed trait Gear
  case object Climbing extends Gear
  case object Torch extends Gear
  case object Neither extends Gear

  sealed trait Terrain { def gear: Set[Gear] }
  case object Rocky extends Terrain { val gear = Set(Climbing, Torch) }
  case object Wet extends Terrain { val gear = Set(Climbing, Neither) }
  case object Narrow extends Terrain { val gear = Set(Torch, Neither) }

  object Terrain {
    def typeOf(i: Int): Terrain = i % 3 match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }
  }

  def parseInput(lines: List[String]): Model = {
    val depth = lines.head.split(" ")(1).toInt
    val target = lines(1).split(" ")(1).split(",").map(_.toInt)
    Model(depth, Point(target(0), target(1)))
  }

  private def geoIndex(p: Point, target: Point, erosion: Point => Int): Int = p match {
    case Point(0, 0)|`target` => 0
    case Point(x, 0) => x * 16807
    case Point(0, y) => y * 48271
    case Point(x, y) => erosion(Point(x-1, y)) * erosion(Point(x, y-1))
  }

  private def erosionLevel(geoIndex: Int, depth: Int) = (geoIndex + depth) % 20183

  def riskLevel(model: Model): Int = {
    val coords = for (x <- 0 to model.target.x; y <- 0 to model.target.y) yield Point(x, y)
    coords
      .foldLeft(Map.empty[Point, Int])((m, p) => m + (p -> erosionLevel(geoIndex(p, model.target, m), model.depth)))
      .values.map(_ % 3).sum
  }

  def fastestRoute(model: Model): Int = {
    val map = mutable.Map.empty[Point, Int]
    def erosion(point: Point): Int = map.getOrElseUpdate(point, erosionLevel(geoIndex(point, model.target, erosion), model.depth))
    @annotation.tailrec
    def traverseTime(time: Int, steps: List[(Point, Gear)], hist: Map[(Point, Gear), Int]): Map[(Point, Gear), Int] = steps match {
      case (point, gear) :: t =>
        val quickMoves = point.adjacents.filter(p => Terrain.typeOf(erosion(p)).gear.contains(gear)).map(p => (p, gear) -> (time + 1)).toMap
        val swapGear = (point, (Terrain.typeOf(erosion(point)).gear - gear).head) -> (time + 7)
        traverseTime(time, t, hist ++ (quickMoves + swapGear).filter(m => hist.getOrElse(m._1, Int.MaxValue) > m._2))
      case Nil => hist
    }
    @annotation.tailrec
    def go(time: Int, steps: Map[(Point, Gear), Int]): Int = {
      if (steps.keySet.contains((model.target, Torch))) steps((model.target, Torch))
      else go(time + 1, traverseTime(time, steps.toList.filter(_._2 == time).map(_._1), steps))
    }

    go(0, Map((Point(0, 0), Torch) -> 0, (Point(0, 0), Neither) -> 7))
  }

  val testModel = Model(510, Point(10, 10))
  val model = parseInput(readFile())
  println(riskLevel(model))
  println(Timer.time(fastestRoute(model)))

}
