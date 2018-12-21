package exercises

import exercises.Day16.Op
import exercises.Day19.Model

object Day21 extends DayN {

  override val num: Int = 21

  @annotation.tailrec
  private def longLoop(n: Int, regs: Array[Int], count: Int): (Array[Int], Int) = {
    if ((n + 1) * 256 > regs(5)) longLoop(n + 1, regs, count + 7)
    else (Array(regs(0), regs(1), n, (n + 1) * 256, 26, regs(5)), count + 6)
  }

  @annotation.tailrec
  def firstCheck(model: Model, acc: Array[Int]): Int =
    if (acc(model.insReg) < 0 || acc(model.insReg) >= model.commands.length) acc(0)
    else {
      val command = model.commands(acc(model.insReg))
      val after = Op.runOp(Day19.chooseOp(command.op), command, acc)
      if (acc(model.insReg) == 28) after(1)
      else firstCheck(model, after.updated(model.insReg, after(model.insReg) + 1))
    }

  @annotation.tailrec
  def lastCheck(model: Model, acc: Array[Int], prev: Int, hist: Set[Int]): Int =
    if (acc(model.insReg) < 0 || acc(model.insReg) >= model.commands.length) acc(1)
    else {
      val command = model.commands(acc(model.insReg))
      val after = Op.runOp(Day19.chooseOp(command.op), command, acc)
      if (acc(model.insReg) == 28 && hist.contains(after(1))) prev
      else if (acc(model.insReg) == 28) lastCheck(model, after.updated(model.insReg, after(model.insReg) + 1), after(1), hist + after(1))
      else lastCheck(model, after.updated(model.insReg, after(model.insReg) + 1), prev, hist)
    }

  val model = Day19.parseInput(readFile())
  println(firstCheck(model, Array.fill(6)(0)))
  println(lastCheck(model, Array.fill(6)(0), -1, Set.empty))
}
