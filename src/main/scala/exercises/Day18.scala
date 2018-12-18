package exercises

import scala.collection.mutable

object Day18 extends DayN {

  override val num: Int = 18

  case class Point(x: Int, y: Int)

  def parseInput(lines: List[String]): Array[Array[Char]] = lines.map(_.toCharArray).toArray

  private def countAdjacents(p: Point, state: Array[Array[Char]], value: Char): Int = {
    val ps = for (x <- -1 to 1; y <- -1 to 1) yield Point(p.x + x, p.y + y)
    (ps.toSet - p).count(a => {
      if (a.x < 0 || a.x >= state.length || a.y < 0 || a.y >= state.length) false
      else state(a.y)(a.x) == value
    })
  }

  private def simulate(state: Array[Array[Char]]): Array[Array[Char]] = {
    val coords = for (x <- state.indices; y <- state.indices) yield Point(x, y)
    val newstate = mutable.ArrayBuffer.fill(state.length, state.length)('.')
    coords.foreach(p => {
      newstate(p.y)(p.x) = state(p.y)(p.x) match {
        case '.' => if (countAdjacents(p, state, '|') >= 3) '|' else '.'
        case '|' => if (countAdjacents(p, state, '#') >= 3) '#' else '|'
        case '#' => if (countAdjacents(p, state, '#') >= 1 && countAdjacents(p, state, '|') >= 1) '#' else '.'
      }
    })
    newstate.map(_.toArray).toArray
  }

  def simulateUntil(state: Array[Array[Char]], iterations: Long): Long = {
    @annotation.tailrec
    def go(minute: Long, s: Array[Array[Char]], ss: Map[String, Long]): Array[Array[Char]] = {
      if (minute > iterations) s
      else {
        val res = simulate(s)
        val asString = res.map(_.mkString).mkString("\n")
        if (ss.keySet.contains(asString)) {
          val foundMin = ss(asString)
          val mod = (iterations - minute) % (minute - foundMin)
          ss.find { case (_, m) => m == foundMin + mod }.map(_._1.split("\n").toList).map(parseInput).get
        }
        else go(minute + 1, res, ss + (asString -> minute))
      }
    }

    val res = go(1L, state, Map())
    val wooded = res.map(_.count(_ == '|')).sum
    val lumber = res.map(_.count(_ == '#')).sum
    wooded * lumber
  }

  val state = parseInput(readFile())
  println(simulateUntil(state, 10))
  println(simulateUntil(state, 1000000000L))

}
