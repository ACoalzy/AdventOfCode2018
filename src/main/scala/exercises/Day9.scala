package exercises

object Day9 {

  case class Rules(players: Int, marbles: Int)

  private case class Node(value: Long, var left: Option[Node], var right: Option[Node])

  def parseInput(s: String): Rules = {
    val splits = s.split(" ")
    Rules(splits(0).toInt, splits(6).toInt)
  }

  private def move(clockwise: Boolean, distance: Int, start: Node): Node = {
    (0 until distance).foldLeft(start)((n, _) => if (clockwise) n.right.get else n.left.get)
  }

  def highestScore(rules: Rules): Long = {
    val players = Stream.continually(0 until rules.players).flatten
    val game = (0 to rules.marbles).zip(players).foldLeft(Map.empty[Int, Long], Option.empty[Node]) {
      case ((scores, None), (marble, player)) =>
        (scores, Some(Node(marble, None, None)))
      case ((scores, Some(current @ Node(_, None, None))), (marble, player)) =>
        val nNode = Node(marble, Some(current), Some(current))
        current.left = Some(nNode)
        current.right = Some(nNode)
        (scores, Some(nNode))
      case ((scores, Some(current)), (marble, player)) =>
        if (marble % 23 == 0) {
          val toBeRemoved = move(false, 7, current)
          toBeRemoved.right.get.left = toBeRemoved.left
          toBeRemoved.left.get.right = toBeRemoved.right
          val score = scores.getOrElse(player, 0L) + toBeRemoved.value + marble
          (scores + (player -> score), toBeRemoved.right)
        } else {
          val moved = move(true, 1, current)
          val nNode = Node(marble, Some(moved), moved.right)
          nNode.right.get.left = Some(nNode)
          moved.right = Some(nNode)
          (scores, Some(nNode))
        }
    }

    game._1.values.max
  }

}
