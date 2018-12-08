package exercises

object Day8 {

  case class Node(children: List[Node], metadata: List[Int])

  def parseInput(s: String): List[Int] = s.split(" ").map(_.toInt).toList

  def parseNode(ints: List[Int]): (Node, List[Int]) = {
    val childCount = ints.head
    val metaCount = ints(1)
    val (remaining, children) = (0 until childCount).foldLeft(ints.drop(2), List.empty[Node]) { case ((is, ns), _) =>
      val nn = parseNode(is)
      (nn._2, nn._1 :: ns)
    }
    val meta = remaining.take(metaCount)
    (Node(children.reverse, meta), remaining.drop(metaCount))
  }

  def sumMetadata(root: Node): Int = {
    root.metadata.sum + root.children.map(sumMetadata).sum
  }

  def complicatedSum(root: Node): Int = {
    if (root.children.isEmpty) root.metadata.sum
    else {
      val children = root.children.toVector
      root.metadata.flatMap(m => children.lift(m-1)).map(complicatedSum).sum
    }
  }

}
