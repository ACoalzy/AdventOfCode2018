package exercises

import util.Timer

import scala.collection.mutable

object Day20 extends DayN {

  override val num: Int = 20

  object Direction {
    def move(p: Point, direction: Char) = direction match {
      case 'N' => p.copy(y = p.y - 1)
      case 'E' => p.copy(x = p.x + 1)
      case 'S' => p.copy(y = p.y + 1)
      case 'W' => p.copy(x = p.x - 1)
    }
  }

  case class Point(x: Int, y: Int)
  case class Reset(loc: Point, parents: List[Point])
  case class State(loc: Point, steps: List[Char], parents: List[Point], resets: List[List[Reset]])

  def buildMap(steps: String, start: Point): Map[Point, Set[Point]] = {
    @annotation.tailrec
    def go(
      loc: Point,
      steps: List[Char],
      parents: List[Point],
      resets: List[List[Reset]],
      states: List[State],
      hist: Map[Point, List[List[Char]]],
      acc: Map[Point, Set[Point]]
    ): Map[Point, Set[Point]] = steps match {
      case h :: t =>
        h match {
          case 'N'|'E'|'S'|'W' =>
            val next = Direction.move(loc, h)
            val links = List(loc -> (acc.getOrElse(loc, Set.empty) + next), next -> (acc.getOrElse(next, Set.empty) + loc)).toMap
            go(next, t, parents, resets, states, hist, acc ++ links)
          case '(' => go(loc, t, loc :: parents, List.empty :: resets, states, hist, acc)
          case '|' => go(parents.head, t, parents, (Reset(loc, parents.tail) :: resets.head) :: resets.tail, states, hist, acc)
          case ')' =>
            val newStates = resets.head.map(r => State(r.loc, t, r.parents, resets.tail))
            val notBeen = newStates.map(_.loc).distinct
              .filterNot(p => p == loc)
              .flatMap(p => newStates.find(s => s.loc == p))
              .filterNot(s => hist.getOrElse(s.loc, List.empty).contains(s.steps))
            go(loc, t, parents.tail, resets.tail, notBeen ::: states, hist ++ notBeen.map(s => s.loc -> (s.steps :: hist.getOrElse(s.loc, List.empty))).toMap, acc)
        }
      case Nil => states match {
        case h :: t => go(h.loc, h.steps, h.parents, h.resets, t, hist, acc)
        case Nil => acc
      }
    }

    go(start, steps.toList, List.empty, List.empty, List.empty, Map.empty, Map.empty)
  }

  def mostDoors(start: Point, map: Map[Point, Set[Point]]): Int = {
    @annotation.tailrec
    def go(list: List[(Point, Int)], visited: Set[Point], latest: Int): Int = list match {
      case h :: t => go(t ::: map(h._1).diff(visited).map((_, h._2 + 1)).toList, visited + h._1, h._2)
      case Nil => latest
    }
    go(List((start, 0)), Set.empty, 0)
  }

  def xDoorsAway(start: Point, map: Map[Point, Set[Point]], limit: Int): Int = {
    @annotation.tailrec
    def go(list: List[(Point, Int)], visited: Map[Point, Int]): Map[Point, Int] = list match {
      case h :: t => go(t ::: map(h._1).diff(visited.keySet).map((_, h._2 + 1)).toList, visited + (h._1 -> h._2))
      case Nil => visited
    }
    go(List((start, 0)), Map.empty).count(pi => pi._2 >= limit)
  }

  private def prettyPrint(map: Map[Point, Set[Point]], start: Point): Unit = {
    val minX = map.keySet.minBy(_.x).x
    val minY = map.keySet.minBy(_.y).y
    val maxX = map.keySet.maxBy(_.x).x
    val maxY = map.keySet.maxBy(_.y).y

    val grid = mutable.ArrayBuffer.fill(3 + ((maxY - minY) * 2), 3 + ((maxX - minX) * 2))('#')
    map.keySet.foreach(p => grid(1 + (p.y - minY)*2)(1 + (p.x - minX)*2) = '.')
    map.foreach { case (p, ps) => ps.foreach(p2 => {
      if (p.x == p2.x) grid((Math.max(p.y, p2.y) - minY) * 2)(1 + (p.x - minX)*2) = '-'
      else grid(1 + (p.y - minY)*2)((Math.max(p.x, p2.x) - minX) * 2) = '|'
    })}
    grid(1 + (start.y - minY)*2)(1 + (start.x - minX)*2) = 'X'

    println(grid.map(_.mkString).mkString("\n"))
  }

  val start = Point(0, 0)
  val map = Timer.time(buildMap(readFile().head.drop(1).dropRight(1), start))
  prettyPrint(map, start)
  println(mostDoors(start, map))
  println(xDoorsAway(start, map, 1000))
}
