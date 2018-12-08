package exercises

object Day8 {

  case class Node(children: List[Node], metadata: List[Int])

  def parseInput(s: String): List[Int] = s.split(" ").map(_.toInt).toList

  @annotation.tailrec
  def parseNode(
    ints: List[Int],
    childCounts: List[Int],
    metaCounts: List[Int],
    children: List[List[Node]],
    latestNode: Option[Node]
  ): Option[Node] = (ints, childCounts, metaCounts, children) match {
    case (_, cch :: cct, mh :: mt, ch :: ct) if cch == 0 =>
      val node = Node(ch.reverse, ints.take(mh))
      (cct, ct) match {
        case (pcch :: cctt, pch :: ctt) =>
          parseNode(ints.drop(mh), (pcch - 1) :: cctt, mt, (node :: pch) :: ctt, Some(node))
        case _ =>
          parseNode(ints.drop(mh), cct, mt, ct, Some(node))
      }
    case (cc :: mc :: t, ccs, mcs, cs) =>
      parseNode(t, cc :: ccs, mc :: mcs, Nil :: cs, latestNode)
    case (Nil, _, _, _) => latestNode
    case _ => None
  }

  @annotation.tailrec
  def sumMetadata(stack: List[Node], res: Int): Int = stack match {
    case h :: t => sumMetadata(h.children ++ t, res + h.metadata.sum)
    case Nil => res
  }

  @annotation.tailrec
  def complicatedSum(stack: List[Node], res: Int): Int = stack match {
    case h :: t =>
      if (h.children.isEmpty) complicatedSum(t, res + h.metadata.sum)
      else {
        val children = h.children.toArray
        complicatedSum(h.metadata.flatMap(m => children.lift(m-1)) ++ t, res)
      }
    case Nil => res
  }

}
