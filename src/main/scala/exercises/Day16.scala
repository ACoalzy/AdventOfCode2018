package exercises

object Day16 extends DayN {

  override val num: Int = 16

  trait OpCommand {
    def a: Int
    def b: Int
    def c: Int
  }
  case class Command(op: Int, a: Int, b: Int, c: Int) extends OpCommand
  case class Sample(before: Array[Int], command: Command, after: Array[Int])

  sealed trait Op
  case object Addr extends Op
  case object Addi extends Op
  case object Mulr extends Op
  case object Muli extends Op
  case object Banr extends Op
  case object Bani extends Op
  case object Borr extends Op
  case object Bori extends Op
  case object Setr extends Op
  case object Seti extends Op
  case object Gtir extends Op
  case object Gtri extends Op
  case object Gtrr extends Op
  case object Eqir extends Op
  case object Eqri extends Op
  case object Eqrr extends Op

  object Op {
    val ops: List[Op] = List(Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr)

    private def b2i(b: Boolean): Int = if (b) 1 else 0

    def runOp(op: Op, command: OpCommand, before: Array[Int]): Array[Int] = {
      val (a, b, c) = (command.a, command.b, command.c)
      val func = op match {
        case Addr => before(a) + before(b)
        case Addi => before(a) + b
        case Mulr => before(a) * before(b)
        case Muli => before(a) * b
        case Banr => before(a) & before(b)
        case Bani => before(a) & b
        case Borr => before(a) | before(b)
        case Bori => before(a) | b
        case Setr => before(a)
        case Seti => a
        case Gtir => b2i(a > before(b))
        case Gtri => b2i(before(a) > b)
        case Gtrr => b2i(before(a) > before(b))
        case Eqir => b2i(a == before(b))
        case Eqri => b2i(before(a) == b)
        case Eqrr => b2i(before(a) == before(b))
      }
      before.updated(c, func)
    }
  }

  def parseSamples(lines: List[String]): List[Sample] = {
    val samples = lines.grouped(4).filter(l => l.head.startsWith("Before")).map(l => {
      def register(s: String) = s.split("\\[")(1).dropRight(1).split(", ").map(_.toInt)
      val bits = l(1).split(" ").map(_.toInt)
      Sample(register(l(0)), Command(bits(0), bits(1), bits(2), bits(3)), register(l(2)))
    })

    samples.toList
  }

  def parseProgram(lines: List[String]): List[Command] = {
    @annotation.tailrec
    def removeSamples(lines: List[String]): List[String] = {
      if (lines.head.startsWith("Before")) removeSamples(lines.drop(4))
      else lines.drop(2)
    }

    removeSamples(lines).map(l => {
      val bits = l.split(" ").map(_.toInt)
      Command(bits(0), bits(1), bits(2), bits(3))
    })
  }

  def behaveLike(samples: List[Sample], count: Int): Int =
    samples.count(s => Op.ops.map(op => Op.runOp(op, s.command, s.before)).count(_ sameElements s.after) >= count)

  def opMap(samples: List[Sample]): Map[Int, Op] = {
    val options = samples.map(s => s -> Op.ops.filter(op => Op.runOp(op, s.command, s.before) sameElements s.after)).toMap
    @annotation.tailrec
    def go(options: Map[Sample, List[Op]], acc: Map[Int, Op]): Map[Int, Op] = {
      val figured = options.filter(o => o._2.size == 1).map(o => o._1.command.op -> o._2.head)
      val figuredOps = figured.values.toSet
      val remaining = options.filterNot(o => figured.keySet.contains(o._1.command.op)).mapValues(_.filterNot(op => figuredOps.contains(op))).filter(_._2.nonEmpty)
      if (remaining.nonEmpty) go(remaining, figured ++ acc)
      else figured ++ acc
    }

    go(options, Map())
  }

  val samples = parseSamples(readFile())
  println(behaveLike(samples, 3))
  val opCodeMap = opMap(samples)
  val testProgram = parseProgram(readFile())
  println(testProgram.foldLeft(Array(0, 0, 0, 0))((z, c) => Op.runOp(opCodeMap(c.op), c, z)).mkString(", "))
}
