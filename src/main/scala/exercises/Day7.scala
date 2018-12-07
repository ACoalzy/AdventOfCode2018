package exercises

object Day7 {

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

  sealed trait WorkerState
  case class Working(id: String, finish: Int) extends WorkerState
  case object Waiting extends WorkerState

  object WorkerState {
    def reset(w: WorkerState, t: Int): WorkerState = w match {
      case Working(_, time) if time > t => Waiting
      case w => w
    }

    def waiting(w: WorkerState): Boolean = w match {
      case Waiting => true
      case _ => false
    }
  }

  def navigateTreeWithHelpersTime(nodes: Seq[Node], helpers: Int, baseDelay: Int): Int = {
    val ids = nodes.flatMap(n => Seq(n.parent, n.child)).toSet
    val parentMap = ids.map(c => c -> nodes.filter(_.child == c).map(_.parent)).toMap
    val delayMap = ids.toList.sorted.zipWithIndex.map { case (s, i) => s -> (i + baseDelay + 1) }.toMap

    @annotation.tailrec
    def go(history: Set[String], workers: List[WorkerState], time: Int): Int = {
      val (newHistory, resetWorkers, workingIds) = workers.foldLeft(history, List.empty[WorkerState], Set.empty[String]) {
        case ((h, ws, wids), w) =>
          w match {
            case Working(id, t) if t <= time => (h + id, Waiting :: ws, wids)
            case s @ Working(id, _) => (h, s :: ws, wids + id)
            case s => (h, s :: ws, wids)
          }
      }
      val options = ids.filter(id =>
          !newHistory.contains(id) && !workingIds.contains(id) && parentMap(id).forall(newHistory.contains)
        ).toList.sorted

      val newWorkers = resetWorkers.foldLeft(List.empty[WorkerState], options)((acc, w) => (w, acc._2) match {
        case (Waiting, h :: t) => (Working(h, time + delayMap(h)) :: acc._1, t)
        case _ => (w :: acc._1, acc._2)
      })._1

      if (newHistory.size == ids.size) time
      else go(newHistory, newWorkers, time + 1)
    }

    go(Set(), List.fill(helpers)(Waiting), 0)
  }

}
