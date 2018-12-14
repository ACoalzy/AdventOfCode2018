package exercises

import scala.collection.mutable

object Day3 extends DayN {

  override val num: Int = 3

  private val gridWidth = 1000

  case class Coord(x: Int, y: Int)

  case class Claim(id: Int, topLeft: Coord, dimensions: Coord) {
    val bottomRight: Coord = Coord((topLeft.x + dimensions.x) - 1, (topLeft.y + dimensions.y) - 1)

    def forEachSquare[A](f: (Int, Int) => A) = for {
      x <- topLeft.x to bottomRight.x
      y <- topLeft.y to bottomRight.y
    } yield f(x, y)

    def contains(coord: Coord): Boolean =
      topLeft.x > coord.x || bottomRight.x < coord.x || topLeft.y > coord.y || bottomRight.y < coord.y
  }

  def parseInput(seq: Seq[String]): Seq[Claim] = seq.map(s => {
    val parts = "\\d+".r.findAllIn(s).map(_.toInt).toArray
    Claim(parts(0), Coord(parts(1), parts(2)), Coord(parts(3), parts(4)))
  })

  private def generateGrid(claims: Seq[Claim]) = {
    val grid = mutable.ArrayBuffer.fill(gridWidth, gridWidth)(0)
    claims.foreach(_.forEachSquare((x, y) => grid(x)(y) = grid(x)(y) + 1))
    grid.map(_.toArray).toArray
  }

  def overlapCount(claims: Seq[Claim]): Int = generateGrid(claims).flatten.count(_ > 1)

  def findNoOverlapSquares(claims: Seq[Claim]): Seq[Int] = {
    val grid = generateGrid(claims)
    claims.filter(_.forEachSquare((x, y) => grid(x)(y) == 1).reduce(_ && _)).map(_.id)
  }

  def findNoOverlapSquaresAlt(claims: Seq[Claim]): Seq[Int] = {
    val grid = generateGrid(claims)
    val coords = for (x <- grid.indices; y <- grid(0).indices) yield Coord(x, y)

    coords.filter(c => grid(c.x)(c.y) > 1)
      .foldLeft(mutable.ArrayBuffer(claims: _*))((cs, coord) => cs.filter(_.contains(coord)))
      .map(_.id).toList
  }

  val claims = Day3.parseInput(readFile())
  println(Day3.overlapCount(claims))
  println(Day3.findNoOverlapSquares(claims))

}
