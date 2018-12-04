package exercises

object Day2 {

  def getChecksum(seq: Seq[String]): Int = {
    def countContainsFreq(seq: Seq[Map[Char, Int]], freq: Int): Int = seq.count(_.values.exists(_ == freq))
    val occurrences = seq.map(_.groupBy(identity).mapValues(_.length))
    countContainsFreq(occurrences, 2) * countContainsFreq(occurrences, 3)
  }

  def commonLetters(s1: String, s2: String): Seq[Char] = s1.intersect(s2)

  def findBoxPair(seq: Seq[String]): Option[(String, String)] = {
    val pairs = for (x <- seq; y <- seq) yield (x, y)
    pairs.find { case (s1, s2) => s1.intersect(s2).length == s1.length - 1 }
  }

}
