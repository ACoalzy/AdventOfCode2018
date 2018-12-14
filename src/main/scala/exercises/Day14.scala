package exercises

object Day14 extends DayN {

  override val num: Int = 14

  private case class State(digits: Seq[Int], i1: Int, i2: Int)

  private def step(buffer: Seq[Int], i1: Int, i2: Int): State = {
    val (v1, v2) = (buffer(i1), buffer(i2))
    val v3 = v1 + v2
    val (d1, d2) = (v3 / 10 % 10, v3 % 10)
    val ds = if (d1 > 0) Seq(d1, d2) else Seq(d2)
    def updateIndex(i: Int, v: Int): Int = (1 + i + v) % (buffer.size + ds.size)
    State(ds, updateIndex(i1, v1), updateIndex(i2, v2))
  }

  def recipesAfterX(a: Int, b: Int, x: Int, recipes: Int) = {
    @annotation.tailrec
    def go(i1: Int, i2: Int, acc: Vector[Int]): Vector[Int] = {
      if (acc.size >= x + recipes) acc
      else {
        val state = step(acc, i1, i2)
        go(state.i1, state.i2, acc ++ state.digits)
      }
    }

    go(0, 1, Vector(a, b)).slice(x, x + recipes).toArray
  }

  def countPriorRecipes(a: Int, b: Int, x: String) = {
    val digits = x.map(_.asDigit)
    @annotation.tailrec
    def go(i1: Int, i2: Int, acc: Vector[Int], lastX: Seq[Int]): Int = {
      if (lastX.startsWith(digits)) acc.size - (digits.size + 1)
      else if (lastX.endsWith(digits)) acc.size - digits.size
      else {
        val state = step(acc, i1, i2)
        val newLast = lastX ++ state.digits
        go(state.i1, state.i2, acc ++ state.digits, newLast.drop(newLast.size - (digits.size + 1)))
      }
    }

    go(0, 1, Vector(a, b), Seq(0, 1))
  }

  println(Day14.recipesAfterX(3, 7, 110201, 10).mkString)
  println(Day14.countPriorRecipes(3, 7, "110201"))

}
