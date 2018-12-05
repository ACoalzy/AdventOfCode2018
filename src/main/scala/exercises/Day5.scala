package exercises

object Day5 {

  private def comparePolarities(a: Char, b: Char) =
    (a.toUpper == b && b.toLower == a) || (a.toLower == b && b.toUpper == a)

  def compress(s: String): Int = {
    @annotation.tailrec
    //note: this flips order of chars every time
    def go(chars: List[Char], res: List[Char]): List[Char] = chars match {
      case h :: Nil => h :: res
      case h :: t if comparePolarities(h, t.head) => go(t.tail, res)
      case h :: t => go(t, h :: res)
      case Nil => res
    }

    @annotation.tailrec
    def recurse(chars: List[Char]): String = {
      val newChars = go(chars, List())
      if (newChars.length == chars.length) chars.mkString("")
      else recurse(newChars)
    }

    recurse(s.toList).length
  }

  def superCompress(s: String): Int = {
    ('a' to 'z').map(c => s.filterNot(sc => sc == c || sc == c.toUpper))
      .map(compress)
      .min
  }

}
