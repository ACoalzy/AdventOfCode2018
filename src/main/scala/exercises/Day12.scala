package exercises

object Day12 extends DayN {

  override val num: Int = 12

  private val padSize = 2
  private val pad = "." * padSize

  case class Model(state: String, shift: Int, rules: Map[String, String])

  private def strip(state: String): String = state.replaceAll("^\\.+|\\.+$", "")

  def parseInput(stateLine: String, rules: List[String]) = {
    val state = stateLine.split(" ")(2)
    Model(state.stripMargin('.'), state.indexOf('#'), rules.map(_.split(" => ")).map(s => s(0) -> s(1)).toMap)
  }

  private def getElement(i: Int, state: String) = if (i < 0 || i >= state.length) '.' else state(i)

  private def sumFlowers(model: Model): Long = model.state.zipWithIndex.filter(_._1 == '#').map(_._2 + model.shift).sum

  @annotation.tailrec
  def runModel(model: Model, generations: Long): Long = {
    val padded = pad + model.state + pad
    if (generations <= 0) sumFlowers(model)
    else {
      val newState = (0 until padded.length).map(i => {
        val surrounding = (-padSize to padSize).map(j => getElement(i+j, padded)).mkString
        model.rules(surrounding)
      }).mkString

      if (strip(newState) == model.state) sumFlowers(model) + (model.state.count(_ == '#') * generations)
      else runModel(Model(strip(newState), model.shift + newState.indexOf('#') - padSize, model.rules), generations - 1)
    }
  }

  val lines = readFile()
  val model = Day12.parseInput(lines.head, lines.drop(2))
  println(Day12.runModel(model, 20))
  println(Day12.runModel(model, 50000000000L))

}
