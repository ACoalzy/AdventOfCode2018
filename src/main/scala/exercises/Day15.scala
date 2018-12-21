package exercises

import scala.collection.mutable

object Day15 extends DayN {

  override val num: Int = 15

  case class Point(x: Int, y: Int) {
    def adjacents: Set[Point] = Set(Point(x + 1, y), Point(x - 1, y), Point(x, y + 1), Point(x, y - 1))
  }

  sealed trait PlayerType
  case object Elf extends PlayerType
  case object Goblin extends PlayerType

  case class Player(race: PlayerType, location: Point, att: Int = 3, hp: Int = 200) {
    def enemies(model: Model): Set[Player] = race match {
      case Elf => model.goblins
      case Goblin => model.elves
    }
    def move(p: Point): Player = Player(race, p, att, hp)
    def adjacentEnemies(model: Model): Set[Player] = enemies(model).filter(g => location.adjacents.contains(g.location))
    def attack(player: Player): Player = player.copy(hp = player.hp - att)
  }

  case class Model(players: Set[Player], walls: Set[Point], floor: Set[Point]) {
    def goblins: Set[Player] = players.filter(_.race match {
      case Goblin => true
      case _ => false
    })

    def elves: Set[Player] = players.filter(_.race match {
      case Elf => true
      case _ => false
    })

    def availableFloor(ps: Set[Point]): Set[Point] = ps.intersect(floor).diff(players.map(_.location))

    def prettyPrint(): Unit = {
      val grid = mutable.ArrayBuffer.fill(walls.maxBy(_.y).y + 1, walls.maxBy(_.x).x + 1)(' ')
      floor.foreach(p => grid(p.y)(p.x) = '.')
      walls.foreach(p => grid(p.y)(p.x) = '#')
      players.foreach(p => {
        val char = p.race match {
          case Elf => 'E'
          case Goblin => 'G'
        }
        grid(p.location.y)(p.location.x) = char
      })
      println(grid.map(_.mkString).mkString("\n"))
    }
  }

  def parseInput(lines: Seq[String]): Model = {
    val grid = lines.map(_.toCharArray)
    val coords = for (x <- lines.head.indices; y <- lines.indices) yield Point(x, y)
    @annotation.tailrec
    def go(cl: List[Point], players: Set[Player], walls: Set[Point], floor: Set[Point]): Model = cl match {
      case h :: t =>
        if (grid(h.y)(h.x) == '#') go(t, players, walls + h, floor)
        else if (grid(h.y)(h.x) == 'G') go(t, players + Player(Goblin, h), walls, floor + h)
        else if (grid(h.y)(h.x) == 'E') go(t, players + Player(Elf, h), walls, floor + h)
        else go(t, players, walls, floor + h)
      case Nil => Model(players, walls, floor)
    }

    go(coords.toList, Set(), Set(), Set())
  }

  private def closestPoint(start: Point, ends: Set[Point], model: Model): Option[Point] = {
    @annotation.tailrec
    def bfs(acc: List[Point], next: List[Point], cache: Set[Point], results: List[Point]): Option[Point] = (acc, next) match {
      case (h :: tail, _) =>
        if (ends.contains(h)) bfs(tail, next, cache + h, h :: results)
        else {
          val adjacents = model.availableFloor(h.adjacents).diff(cache).filterNot(next.contains)
          bfs(tail, next ::: adjacents.toList, cache + h, results)
        }
      case _ if results.nonEmpty => Some(results.minBy(bits => (bits.y, bits.x)))
      case (Nil, Nil) => None
      case _ => bfs(next, List.empty, cache, results)
    }
    bfs(List(start), List.empty, Set.empty, List.empty)
  }

  private def move(player: Player, model: Model): (Player, Model) = {
    val res = for {
      _ <- Some(player.adjacentEnemies(model)).filter(_.isEmpty)
      possiblePoints <- Some(model.availableFloor(player.enemies(model).flatMap(p => p.location.adjacents))).filter(_.nonEmpty)
      closest <- closestPoint(player.location, possiblePoints, model)
      newLoc <- closestPoint(closest, player.location.adjacents, model)
    } yield (player.move(newLoc), model.copy(players = model.players.map(p => if (p == player) p.move(newLoc) else p)))
    res.getOrElse((player, model))
  }

  private def attack(player: Player, model: Model): (Option[Player], Model) = {
    val enemies = player.adjacentEnemies(model)
    if (enemies.nonEmpty) {
      val chosenEnemy = enemies.minBy(e => (e.hp, e.location.y, e.location.x))
      val attacked = player.attack(chosenEnemy)
      val (victim, corpse) = if (attacked.hp > 0) (Some(attacked), None) else (None, Some(attacked))
      (corpse, model.copy(players = model.players.flatMap(p => if (p == chosenEnemy) victim else Some(p))))
    } else (None, model)
  }

  @annotation.tailrec
  def battle(model: Model, round: Int): (Model, Int) = {
   @annotation.tailrec
    def turn(playersRemaining: List[Player], model: Model, corpses: Set[Point]): (Boolean, Model) = playersRemaining match {
      case player :: t =>
        if (corpses.contains(player.location)) turn(t, model, corpses)
        else if (player.enemies(model).map(_.location).diff(corpses).isEmpty && t != Nil) (false, model)
        else {
          val (movedPlayer, movedModel) = move(player, model)
          val (corpse, attackedModel) = attack(movedPlayer, movedModel)
          turn(t, attackedModel, corpse.map(corpses + _.location).getOrElse(corpses))
        }
      case Nil => (true, model)
    }
    val (full, turnRes) = turn(model.players.toList.sortBy(p => (p.location.y, p.location.x)), model, Set())
    if (turnRes.elves.isEmpty || turnRes.goblins.isEmpty) (turnRes, if (!full) round - 1 else round)
    else battle(turnRes, round + 1)
  }

  @annotation.tailrec
  def elfHelper(attack: Int, model: Model): (Model, Int) = {
    val helped = model.copy(players = model.goblins ++ model.elves.map(e => e.copy(att = attack)))
    val (game, rounds) = battle(helped, 1)
    if (game.elves.size == model.elves.size) (game, rounds)
    else elfHelper(attack + 1, model)
  }

  val model = parseInput(readFile())
  val (battlefield, rounds) = battle(model, 1)
  val sum = battlefield.players.toList.map(_.hp).sum
  println(s"$sum * $rounds = ${sum * rounds}")

  val (elvenField, elfRounds) = elfHelper(4, model)
  val elfSum = elvenField.players.toList.map(_.hp).sum
  println(s"$elfSum * $elfRounds = ${elfSum * elfRounds}")
}
