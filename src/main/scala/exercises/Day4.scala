package exercises

import scala.collection.mutable

object Day4 {

  sealed trait EntryDetail
  case class GuardStart(id: Int) extends EntryDetail
  case object Sleep extends EntryDetail
  case object Awake extends EntryDetail

  case class Entry(date: String, minute: Int, details: EntryDetail)

  def parseInput(seq: Seq[String]): Seq[Entry] = {
    seq.map(s => {
      val details = s.split("]")(1).drop(1) match {
        case s if s.contains("begins") => GuardStart(s.split(" ")(1).drop(1).toInt)
        case s if s.contains("asleep") => Sleep
        case _ => Awake
      }
      Entry(s.split("]")(0).drop(1), s.split("]")(0).split(":")(1).toInt, details)
    }).sortBy(_.date)
  }

  private def sleepMap(entries: Seq[Entry]) = {
    entries.foldLeft(0, -1, mutable.Map[Int, Seq[Int]]()) { case ((prevMin, guardId, sleepMap), e) =>
      if (e.details == Awake) sleepMap.update(guardId, (prevMin until e.minute).toList ++ sleepMap.getOrElse(guardId, Seq()))
      e.details match {
        case GuardStart(id) => (e.minute, id, sleepMap)
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

}
