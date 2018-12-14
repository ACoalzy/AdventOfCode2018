package exercises

object Day7 extends DayN {

  override val num: Int = 7

  case class Node(parent: String, child: String)

  def parseInput(lines: Seq[String]): Seq[Node] = lines.map(l => Node(l.split(" ")(1), l.split(" ")(7)))

  def navigateTree(nodes: Seq[Node]): String = {
    val ids = nodes.flatMap(n => Seq(n.parent, n.child)).toSet
    val parentMap = ids.map(c => c -> nodes.filter(_.child == c).map(_.parent)).toMap

    @annotation.tailrec
    def go(history: Set[String], acc: String): String =
      ids.filter(id => !history.contains(id) && parentMap(id).forall(history.contains)).toList.sorted match {
        case h :: _ => go(history + h, acc + h)
        case Nil => acc
      }

    go(Set(), "")
  }

  case class Worker(id: String, finish: Int)

  def navigateTreeWithHelpersTime(nodes: Seq[Node], helpers: Int, baseDelay: Int): Int = {
    val ids = nodes.flatMap(n => Seq(n.parent, n.child)).toSet
    val parentMap = ids.map(c => c -> nodes.filter(_.child == c).map(_.parent)).toMap
    val delayMap = ids.toList.sorted.zipWithIndex.map { case (s, i) => s -> (i + baseDelay + 1) }.toMap

    @annotation.tailrec
    def go(history: Set[String], workers: List[Worker], time: Int): Int = {
      val (finished, working) = workers.partition(_.finish <= time)
      val newHistory = history ++ finished.map(_.id)
      if (newHistory.size == ids.size) time
      else {
        val newWorkers = working ++ ids.diff(newHistory ++ working.map(_.id))
          .filter(id => parentMap(id).forall(newHistory.contains))
          .toList.sorted
          .take(helpers - working.size)
          .map(o => Worker(o, time + delayMap(o)))

        go(newHistory, newWorkers, newWorkers.minBy(_.finish).finish)
      }
    }

    go(Set(), List.empty[Worker], 0)
  }

  val nodes = Day7.parseInput(readFile())
  println(Day7.navigateTree(nodes))
  println(Day7.navigateTreeWithHelpersTime(nodes, 5, 60))

}
