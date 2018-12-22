package exercises

import util.Timer

import scala.collection.mutable

object Day22 extends DayN {

  override val num: Int = 22

  case class Point(x: Int, y: Int) {
    def adjacents: Set[Point] = Set(Point(x + 1, y), Point(x - 1, y), Point(x, y + 1), Point(x, y - 1)).filter(_.x >= 0).filter(_.y >= 0)
    def dist(p: Point) = Math.abs(x - p.x) + Math.abs(y - p.y)
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

  case class Step(p: Point, g: Gear, t: Int)

  def fastestRoute(model: Model): Int = {
    val map = mutable.Map.empty[Point, Int]
    def erosion(point: Point): Int = map.getOrElseUpdate(point, erosionLevel(geoIndex(point, model.target, erosion), model.depth))
    val queue = mutable.PriorityQueue[Step]()(Ordering.by((s: Step) => s.t + s.p.dist(model.target)).reverse)
    @annotation.tailrec
    def go(hist: Set[(Point, Gear)]): Int = queue.dequeue match {
      case Step(target, Torch, t) if target == model.target => t
      case Step(point, gear, _) if hist.contains((point, gear)) => go(hist)
      case Step(point, gear, t) =>
        val moves = point.adjacents.filter(p => Terrain.typeOf(erosion(p)).gear.contains(gear)).map(p => Step(p, gear, t + 1)).toList
        val swapGear = Step(point, (Terrain.typeOf(erosion(point)).gear - gear).head, t + 7)
        queue.enqueue((swapGear :: moves).filterNot(s => hist.contains((s.p, s.g))): _*)
        go(hist + (point -> gear))
    }
    queue.enqueue(Step(Point(0, 0), Torch, 0))
    go(Set.empty)
  }

  val testModel = Model(510, Point(10, 10))
  val model = parseInput(readFile())
  println(riskLevel(model))
  println(fastestRoute(model))

}
