package exercises

object Day1 {

  sealed trait Function
  case object Add extends Function
  case object Subtract extends Function

  case class Instruction(function: Function, amount: Long)

  object Instruction {
    def parse(s: String) = {
      val f = s.take(1) match {
        case "+" => Add
        case "-" => Subtract
      }
      val a = s.drop(1).toLong
      Instruction(f, a)
    }
  }

  private def executeInstruction(freq: Long, i: Instruction): Long = i.function match {
    case Add => freq + i.amount
    case Subtract => freq - i.amount
  }

  def processFrequencies(changes: Seq[String]): Long = {
    changes.map(Instruction.parse).foldLeft(0L)(executeInstruction)
  }

  def firstDuplicateFrequency(changes: Seq[String]): Long = {
    @annotation.tailrec
    def go(stream: Stream[Instruction], history: Set[Long], frequency: Long): Long = {
      val newFreq = executeInstruction(frequency, stream.head)
      if (history.contains(newFreq)) newFreq
      else go(stream.tail, history + newFreq, newFreq)
    }

    go(Stream.continually(changes.map(Instruction.parse).toStream).flatten, Set(), 0)
  }

}
