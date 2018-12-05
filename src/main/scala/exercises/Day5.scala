package exercises

object Day5 {

  private def comparePolarities(a: Char, b: Char) = a != b && a.toUpper == b.toUpper

  def compress(s: String): String = {
    @annotation.tailrec
    def go(chars: List[Char], res: List[Char]): List[Char] = chars match {
      case h :: Nil => h :: res
      case h :: t if comparePolarities(h, t.head) => if (res.nonEmpty) go(res.head :: t.tail, res.tail) else go(t.tail, res)
      case h :: t => go(t, h :: res)
      case Nil => res
    }

    go(s.toList, List()).mkString("")
  }

  def superCompress(s: String): String = {
    val compressed = compress(s)
    ('a' to 'z')
      .map(c => compressed.filterNot(sc => sc == c || sc == c.toUpper))
      .map(compress)
      .minBy(_.length)
  }

}
