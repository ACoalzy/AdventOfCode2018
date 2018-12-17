package exercises

import scala.collection.mutable

object Day11 extends DayN {

  override val num: Int = 11

  private val size = 300

  private def cellPower(x: Int, y: Int, input: Int): Int = {
    val rackId = x + 10
    ((rackId * y + input) * rackId / 100) % 10 - 5
  }

  private def getIndex(x: Int, y: Int, grid: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]) = if (x < 0 || y < 0) 0 else grid(y)(x)

  private def genSummedGrid(input: Int) = {
    val grid = mutable.ArrayBuffer.fill(size, size)(0)
    for (x <- 0 until size; y <- 0 until size) yield grid(y)(x) = (cellPower(x+1, y+1, input) + getIndex(x, y-1, grid) + getIndex(x-1, y, grid)) - getIndex(x-1, y-1, grid)
    grid
  }

  def largestNxN(input: Int, minN: Int, maxN: Int) = {
    val grid = genSummedGrid(input)
    val combos = for(n <- minN to maxN; x <- 0 to (size - n); y <- 0 to (size - n)) yield(x, y, n)
    combos.foldLeft(Int.MinValue, (0, 0, 0)) { case ((max, (mx, my, mn)), (x, y, n)) =>
      val sum = getIndex(x-1, y-1, grid) + getIndex(x+(n-1), y+(n-1), grid) - getIndex(x+(n-1), y-1, grid) - getIndex(x-1, y+(n-1), grid)
      if (sum > max) (sum, (x+1, y+1, n)) else (max, (mx, my, mn))
    }
  }

  println(largestNxN(5468, 3, 3))
  println(largestNxN(5468, 1, 300))

}
