package exercises

import sun.tools.jconsole.Plotter

object Day23 extends DayN {

  override val num: Int = 23

  case class Point(x: Int, y: Int, z: Int) {
    def dist(that: Point): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    def +(that: Point): Point = Point(x + that.x, y + that.y, z + that.z)
  }

  case class Nanobot(pos: Point, radius: Int) {
    def overlaps(n: Nanobot): Boolean = (pos dist n.pos) <= radius + n.radius
    def contains(cPos: Point): Boolean = (pos dist cPos) <= radius
    def contains(that: Nanobot): Boolean = that.corners.forall(contains)
    def corners: Set[Point] = Set(
      Point(-radius, 0, 0),
      Point(radius, 0, 0),
      Point(0, -radius, 0),
      Point(0, radius, 0),
      Point(0, 0, -radius),
      Point(0, 0, radius),
    ).map(pos + _)
  }

  def withinLargestRadius(nanobots: Seq[Nanobot]): Int = {
    val largestRadius = nanobots.maxBy(_.radius)
    nanobots.count(nanobot => largestRadius.contains(nanobot.pos))
  }

  case class State(used: Set[Nanobot], possible: Set[Nanobot], history: Set[Nanobot])

  def largestGroup(neighbours: Map[Nanobot, Set[Nanobot]]): Set[Nanobot] = {
    @annotation.tailrec
    def bronKerbosch(states: List[State], best: Set[Nanobot]): Set[Nanobot] = states match {
      case h :: t if h.possible.isEmpty && h.history.isEmpty => bronKerbosch(t, if (h.used.size > best.size) h.used else best)
      case h :: t =>
        val options = (h.possible -- neighbours((h.possible ++ h.history).maxBy(neighbours(_).size))).toList
        val combos = options.foldLeft((List.empty[State], h)) { case ((ss, state), n) =>
          val s1 = State(state.used + n, state.possible intersect neighbours(n), state.history intersect neighbours(n))
          val s2 = State(state.used, state.possible - n, state.possible + n)
          (s1 :: ss, s2)
        }
        bronKerbosch(combos._1 ::: t, best)
      case Nil => best
    }

    bronKerbosch(List(State(Set.empty, neighbours.keySet, Set.empty)), Set.empty)
  }

  def closestMostIntersects(bots: List[Nanobot]): Int = {
    val botLinks = bots.map(n => n -> bots.filter(n2 => n2 != n && n.overlaps(n2)).toSet).toMap
    largestGroup(botLinks).map(n => (n.pos dist Point(0, 0, 0)) - n.radius).max
  }

  private val regex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def parseInput(lines: List[String]): List[Nanobot] = lines.map{
    case regex(x, y, z, r) => Nanobot(Point(x.toInt, y.toInt, z.toInt), r.toInt)
  }

  val bots = parseInput(readFile())
  println(withinLargestRadius(bots))
  println(closestMostIntersects(bots))
}
