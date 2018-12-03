package exercises

import scala.collection.mutable

object Day3 {

  case class Coord(x: Int, y: Int)

  case class Claim(id: String, topLeft: Coord, dimensions: Coord) {
    val bottomRight: Coord = Coord((topLeft.x + dimensions.x) - 1, (topLeft.y + dimensions.y) - 1)

    def forEachSquare[A](f: (Int, Int) => A) = for {
      x <- topLeft.x to bottomRight.x
      y <- topLeft.y to bottomRight.y
    } yield f(x, y)
  }

  def parseInput(seq: Seq[String]): Seq[Claim] = seq.map(s => {
    val parts = s.split(" ")
    val tlParts = parts(2).split(",").map(_.replace(":", "").toInt)
    val dimParts = parts(3).split("x").map(_.toInt)
    Claim(parts(0), Coord(tlParts(0), tlParts(1)), Coord(dimParts(0), dimParts(1)))
  })

  private def generateGrid(claims: Seq[Claim]) = {
    val grid = mutable.ArrayBuffer.fill(1000, 1000)(0)
    claims.foreach(_.forEachSquare((x, y) => grid(x)(y) = grid(x)(y) + 1))
    grid.map(_.toArray).toArray
  }

  def inchesOfOverlap(claims: Seq[Claim]): Int = {
    val grid = generateGrid(claims)
    grid.flatten.count(_ > 1)
  }

  def findNoOverlapSquare(claims: Seq[Claim]): Option[String] = {
    val grid = generateGrid(claims)
    def overlapFree(claim: Claim): Boolean = claim.forEachSquare((x, y) => grid(x)(y) == 1).reduce(_ && _)
    @annotation.tailrec
    def go(cs: Seq[Claim]): Option[Claim] = cs match {
      case h :: _ if overlapFree(h) => Some(h)
      case _ :: t => go(t)
      case Nil => None
    }

    go(claims).map(_.id)
  }

}
