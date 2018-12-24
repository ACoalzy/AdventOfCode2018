package exercises

object Day24 extends DayN {

  override val num: Int = 24

  sealed trait Team
  case object Immune extends Team
  case object Infection extends Team

  case class Group(team: Team, units: Int, hp: Int, att: Int, attackType: String, initiative: Int, weaknesses: Set[String], immunities: Set[String]) {
    def potentialDamage(attacker: Group): Int = {
      val damage = attacker.units * attacker.att
      if (weaknesses.contains(attacker.attackType)) damage * 2
      else if (immunities.contains(attacker.attackType)) 0
      else damage
    }

    def attack(target: Group): Group =
      target.copy(units = target.units - target.potentialDamage(this) / target.hp)
  }

  def parseInput(lines: List[String]): Set[Group] = {
    val regex = """(\d+) units each with (\d+) hit points (.*) ?with an attack that does (\d+) (.+) damage at initiative (\d+)""".r
    val regex2 = """(.+) to (.+)""".r
    def parseGroup(team: Team, line: String): Group = {
      line match {
        case regex(units, hp, features, att, attackType, initiative) =>
          val trimmed = if (features.length > 2) features.trim.drop(1).dropRight(1) else features
          val featureMap = trimmed.split("; ").filterNot(_.isEmpty).map { case regex2(role, types) => role -> types.split(", ").toSet }.toMap
          Group(team, units.toInt, hp.toInt, att.toInt, attackType, initiative.toInt, featureMap.getOrElse("weak", Set.empty), featureMap.getOrElse("immune", Set.empty))
      }
    }
    val split = lines.indexOf("")
    val immune = lines.slice(1, split).map(l => parseGroup(Immune, l)).toSet
    val infection = lines.drop(split + 2).map(l => parseGroup(Infection, l)).toSet
    immune ++ infection
  }

  private def targetSelection(model: Set[Group]): Map[Group, Group] = {
    def chooseTarget(g: Group, ts: Set[Group]): Option[Group] =
      if (ts.exists(_.potentialDamage(g) > 0)) Some(ts.maxBy(t => (t.potentialDamage(g), t.att * t.units))) else None
    def targets(groups: Set[Group], targets: Set[Group]): Map[Group, Group] =
      groups.toList.sortBy(g => (g.units * g.att, g.initiative)).reverse
        .foldLeft(Map.empty[Group, Option[Group]])((m, g) => m + (g -> chooseTarget(g, targets diff m.values.flatten.toSet)))
        .collect { case (k, Some(v)) => k -> v }

    val (immune, infection) = model.partition(_.team == Immune)
    val immuneTargets = targets(immune, infection)
    val infectionTargets = targets(infection, immune)
    immuneTargets ++ infectionTargets
  }

  private def attack(model: Set[Group], targets: Map[Group, Group]): Set[Group] = model.toList.sortBy(_.initiative).reverse
    .foldLeft((Set.empty[Group], model.map(g => g -> g).toMap)) { case ((dead, updates), g) => targets.get(g) match {
      case Some(t) if !dead.contains(g) && !dead.contains(t) =>
        val attacked = updates(g).attack(updates(t))
        if (attacked.units > 0) (dead, updates + (t -> attacked))
        else (dead + t, updates - t)
      case _ => (dead, updates)
    }}._2.values.toSet

  @annotation.tailrec
  def simulate(model: Set[Group]): Set[Group] = {
    val targets = targetSelection(model)
    val aftermath = attack(model, targets)
    if (aftermath.forall(_.team == aftermath.head.team))aftermath
    else if (aftermath == model) model
    else simulate(aftermath)
  }

  def simulateUntilImmuneWins(model: Set[Group]): Option[Int] = {
    Stream.from(1)
      .map(i => simulate(model.map(g => if (g.team == Immune) g.copy(att = g.att + i) else g)))
      .find(_.forall(_.team == Immune))
      .map(_.map(_.units).sum)
  }

  val model = parseInput(readFile())
  println(simulate(model).map(_.units).sum)
  println(simulateUntilImmuneWins(model))
}
