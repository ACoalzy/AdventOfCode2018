package exercises

import Day16._

object Day19 extends DayN {

  override val num: Int = 19

  case class Command(op: String, a: Int, b: Int, c: Int) extends OpCommand {
    override def toString: String = s"$op $a $b $c"
  }

  case class Model(insReg: Int, commands: Array[Command])

  def parseInput(lines: Seq[String]): Model = {
    Model(lines.head.split(" ")(1).toInt, lines.tail.map(s => {
      val bits = s.split(" ")
      Command(bits(0), bits(1).toInt, bits(2).toInt ,bits(3).toInt)
    }).toArray)
  }

  private def chooseOp(name: String): Op = name match {
    case "addr" => Addr
    case "addi" => Addi
    case "mulr" => Mulr
    case "muli" => Muli
    case "banr" => Banr
    case "bani" => Bani
    case "borr" => Borr
    case "bori" => Bori
    case "setr" => Setr
    case "seti" => Seti
    case "gtir" => Gtir
    case "gtri" => Gtri
    case "gtrr" => Gtrr
    case "eqir" => Eqir
    case "eqri" => Eqri
    case "eqrr" => Eqrr
  }

  @annotation.tailrec
  def runProgram(model: Model, acc: Array[Int]): Array[Int] =
    if (acc(model.insReg) < 0 || acc(model.insReg) >= model.commands.length) acc
    else {
      val command = model.commands(acc(model.insReg))
      val after = Op.runOp(chooseOp(command.op), command, acc)
      println(command)
      println(after.updated(model.insReg, after(model.insReg) + 1).mkString(", "))
      Thread.sleep(100)
      runProgram(model, after.updated(model.insReg, after(model.insReg) + 1))
    }

  val model = parseInput(readFile())
  println(runProgram(model, Array.fill(6)(0)).mkString(", "))

}
