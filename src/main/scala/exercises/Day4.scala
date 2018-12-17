package exercises

import scala.collection.mutable

object Day4 extends DayN {

  override val num: Int = 4

  sealed trait Entry {
    def date: String
    val minute = date.split(":")(1).toInt
  }
  case class GuardStart(id: Int, date: String) extends Entry
  case class Sleep(date: String) extends Entry
  case class Awake(date: String) extends Entry

  def parseInput(seq: Seq[String]): Seq[Entry] = {
    val regex = "\\[(.*)\\] (.*)".r
    val (guardRgx, sleepRgx, awakeRgx) = ("Guard #(\\d+) begins shift".r, "falls asleep".r, "wakes up".r)

    seq.map {
      case regex(date, detailString) =>
        detailString match {
          case guardRgx(id) => GuardStart(id.toInt, date)
          case sleepRgx() => Sleep(date)
          case awakeRgx() => Awake(date)
        }
    }.sortBy(_.date)
  }

  private def sleepMap(entries: Seq[Entry]) = {
    entries.foldLeft(0, -1, mutable.Map[Int, Seq[Int]]()) { case ((prevMin, guardId, sleepMap), e) =>
      e match {
        case GuardStart(id, _) => (e.minute, id, sleepMap)
        case Awake(_) =>
          sleepMap.update(guardId, (prevMin until e.minute).toList ++ sleepMap.getOrElse(guardId, Seq()))
          (e.minute, guardId, sleepMap)
        case _ => (e.minute, guardId, sleepMap)
      }
    }._3.toMap
  }

  implicit class MapOps[K, V](self: Map[K, Seq[V]]) {
    def maxBySize() = self.maxBy(_._2.size)._1
  }

  implicit class SeqOps[V](self: Seq[V]) {
    def mostFrequent() = self.groupBy(identity).maxBySize
  }

  def sleepiestGuardAndMinute(entries: Seq[Entry]): Int = {
    val map = sleepMap(entries)
    val sleepiestGuard = map.maxBySize
    sleepiestGuard * map(sleepiestGuard).mostFrequent
  }

  def sleepiestMinuteAndGuard(entries: Seq[Entry]): Int = {
    val map = sleepMap(entries)
    val sleepiestMinutes = map.mapValues(_.mostFrequent)
    val sleepiest = sleepiestMinutes.maxBy { case (id, min) => map(id).count(_ == min) }
    sleepiest._1 * sleepiest._2
  }

  val lines = readFile()
  val entries = parseInput(lines)
  println(sleepiestGuardAndMinute(entries))
  println(sleepiestMinuteAndGuard(entries))
}
