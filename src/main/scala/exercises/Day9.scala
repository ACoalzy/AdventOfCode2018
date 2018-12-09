package exercises

object Day9 {

  case class Rules(players: Int, marbles: Int)

  private case class Node(value: Long) {
    self =>
      private var lefty: Node = null
      private var righty: Node = null

      def move(clockwise: Boolean, distance: Int): Node = {
        (0 until distance).foldLeft(self)((n, _) => if (clockwise) n.righty else n.lefty)
      }

      def insertRight(marble: Long): Node = {
        val node = Node(marble)
        node.lefty = self
        node.righty = self.righty
        node.righty.lefty = node
        node.lefty.righty = node
        node
      }

    def remove: (Long, Node) = {
      self.righty.lefty = self.lefty
      self.lefty.righty = self.righty
      (self.value, self.righty)
    }
  }

  private object Node {
    def init(marble: Long): Node = {
      val node = Node(marble)
      node.lefty = node
      node.righty = node
      node
    }
  }

  def parseInput(s: String): Rules = {
    val splits = s.split(" ")
    Rules(splits(0).toInt, splits(6).toInt)
  }

  implicit class VectorOps[A](self: Vector[A]) {
    private def rotateRight(distance: Int) = self.takeRight(distance) ++ self.dropRight(distance)
    private def rotateLeft(distance: Int) = self.drop(distance) ++ self.take(distance)
    def rotate(distance: Int): Vector[A] =
      if (distance >= 0) rotateRight(distance)
      else rotateLeft(distance * -1)
  }

  def highestScoreVector(rules: Rules): Long = {
    val players = Stream.continually(0 until rules.players).flatten
    val game = (0 to rules.marbles).zip(players).foldLeft(Map.empty[Int, Long], Vector.empty[Int]) {
      case ((scores, marbles), (marble, player)) =>
        if (marble > 0 && marble % 23 == 0) {
          val rotated = marbles.rotate(7)
          (scores + (player -> (scores.getOrElse(player, 0l) + marble + rotated.head)), rotated.tail)
        } else (scores, marble +: marbles.rotate(-2))
    }

    game._1.values.max
  }

  def highestScore(rules: Rules): Long = {
    val players = Stream.continually(0 until rules.players).flatten
    val game = (0 to rules.marbles).zip(players).foldLeft(Map.empty[Int, Long], Option.empty[Node]) {
      case ((scores, None), (marble, _)) => (scores, Some(Node.init(marble)))
      case ((scores, Some(current)), (marble, player)) =>
        if (marble % 23 == 0) {
          val (value, node) = current.move(false, 7).remove
          (scores + (player -> (scores.getOrElse(player, 0L) + value + marble)), Some(node))
        } else (scores, Some(current.move(true, 1).insertRight(marble)))
    }

    game._1.values.max
  }

}
