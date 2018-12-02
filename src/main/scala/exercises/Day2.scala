package exercises

object Day2 {

  def getChecksum(seq: Seq[String]): Int = {
    def countOccurrences(seq: Seq[Char]): Map[Char, Int] = seq.groupBy(identity).mapValues(_.size)
    def countContainsFreq(seq: Seq[Map[Char, Int]], freq: Int): Int = seq.count(m => m.values.exists(_ == freq))
    val occurrences = seq.map(s => countOccurrences(s.toCharArray))
    countContainsFreq(occurrences, 2) * countContainsFreq(occurrences, 3)
  }

  private def oneOff(s1: String, s2: String): Boolean = {
    val pairs = s1.toCharArray.zip(s2.toCharArray)
    val count = pairs.foldLeft(0)((b, p) => if (p._1 != p._2) b + 1 else b)
    count == 1
  }

  def commonLetters(s1: String, s2: String): Seq[Char] = {
    val pairs = s1.toCharArray.zip(s2.toCharArray)
    pairs.foldLeft(Seq(): Seq[Char])((b, p) => if (p._1 == p._2) p._1 +: b else b).reverse
  }

  def findBoxPair(seq: Seq[String]): Option[(String, String)] = {
    @annotation.tailrec
    def go(ss: Seq[(String, String)]): Option[(String, String)] = ss match {
      case h :: _ if oneOff(h._1, h._2) => Some((h._1, h._2))
      case _ :: t => go(t)
      case Nil => None
    }
    go(for (x <- seq; y <- seq) yield (x, y))
  }

}
