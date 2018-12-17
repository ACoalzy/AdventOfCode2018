package exercises

object Day13 extends DayN {

  override val num = 13

  private val cartChars = Set('>', '<', '^', 'v')
  private val hTrack = Set('-', '/', '\\', '+')
  private val vTrack = Set('|', '/', '\\', '+')
  private val corners = Set('/', '\\')

  sealed trait Turn
  case object Left extends Turn
  case object Right extends Turn
  case object Straight extends Turn

  case class Cart(x: Int, y: Int, vx: Int, vy: Int, nextTurn: Turn = Left) {
    def nx: Int = x + vx
    def ny: Int = y + vy

    def turn(c: Char): Cart = {
      if ((c == '\\' && vx == 0) || (c == '/' && vy == 0)) Cart(nx, ny, vy, -vx, nextTurn)
      else if ((c == '\\' && vy == 0) || (c == '/' && vx == 0)) Cart(nx, ny, -vy, vx, nextTurn)
      else this
    }

    def junction() = nextTurn match {
      case Left => Cart(nx, ny, vy, -vx, Straight)
      case Right => Cart(nx, ny, -vy, vx, Left)
      case Straight => Cart(nx, ny, vx, vy, Right)
    }
  }

  case class Model(tracks: Array[Array[Char]], carts: List[Cart])

  // didn't read problem spec properly so this is overkill...
  private def underlyingTrack(x: Int, y: Int, grid: Array[Array[Char]]): Char = {
    def get(xx: Int, yy: Int) = if (xx < 0 || yy < 0 || xx >= grid.head.length || yy >= grid.length) ' ' else grid(yy)(xx)
    val (right, left) = (hTrack.contains(get(x+1, y)), hTrack.contains(get(x-1, y)))
    val (up, down) = (vTrack.contains(get(x, y-1)), vTrack.contains(get(x, y+1)))
    if (right && left && up && down) '+'
    else if (right && left) '-'
    else if (up && down) '|'
    else if ((right && down) || (left && up)) '/'
    else if ((left && down) || (right && up)) '\\'
    else ' '
  }

  def parseInput(input: List[String]): Model = {
    val padded = input.map(_.padTo(input.map(_.length).max, ' ').toArray).toArray
    val cartCoords = for (x <- padded.head.indices; y <- padded.indices; cart <- if (cartChars.contains(padded(y)(x))) Some((x, y)) else None) yield cart
    val carts = cartCoords.map(xy => {
      val dir = padded(xy._2)(xy._1) match {
        case '>' => (1, 0)
        case '<' => (-1, 0)
        case '^' => (0, -1)
        case 'v' => (0, 1)
      }
      Cart(xy._1, xy._2, dir._1, dir._2)
    })
    val mutable = padded.map(_.toBuffer)
    cartCoords.foreach(xy => mutable(xy._2)(xy._1) = underlyingTrack(xy._1, xy._2, padded))
    Model(mutable.map(_.toArray), carts.toList)
  }

  private def move(cart: Cart, track: Char) = {
    if (track == '+') cart.junction()
    else if (corners.contains(track)) cart.turn(track)
    else cart.copy(x = cart.nx, y = cart.ny)
  }

  @annotation.tailrec
  def simulateUntilCrash(model: Model): (Int, Int) = {
    @annotation.tailrec
    def tick(remaining: List[Cart], moved: List[Cart]): (List[Cart], Option[(Int, Int)]) = remaining match {
      case cart :: t =>
        def collision(c: Cart) = c.x == cart.nx && c.y == cart.ny
        if (moved.exists(collision) || remaining.exists(collision)) (remaining, Some(cart.nx, cart.ny))
        else tick(t, move(cart, model.tracks(cart.ny)(cart.nx)) :: moved)
      case Nil => (moved, None)
    }

    tick(model.carts, Nil) match {
      case (newCarts, None) => simulateUntilCrash(model.copy(carts = newCarts.sortBy(c => (c.y, c.x))))
      case (_, Some(c)) => c
    }
  }

  @annotation.tailrec
  def lastCartStanding(model: Model): Cart = {
    @annotation.tailrec
    def tick(remaining: List[Cart], moved: List[Cart]): List[Cart] = remaining match {
      case cart :: t =>
        def collision(c: Cart) = c.x == cart.nx && c.y == cart.ny
        (moved.find(collision), remaining.find(collision)) match {
          case (Some(victim), _) => tick(t, moved.filterNot(_ == victim))
          case (_, Some(victim)) => tick(t.filterNot(_ == victim), moved)
          case _ => tick(t, move(cart, model.tracks(cart.ny)(cart.nx)) :: moved)
        }
      case Nil => moved
    }

    tick(model.carts, Nil) match {
      case h :: Nil => h
      case carts => lastCartStanding(model.copy(carts = carts.sortBy(c => (c.y, c.x))))
    }
  }

  val model = parseInput(readFile())
  println(simulateUntilCrash(model))
  println(lastCartStanding(model))

}
