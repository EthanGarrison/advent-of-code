package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.readResourceFile

case class IntCodeProgram(listing: Vector[Long], currLine: Int, relLine: Int = 0, io: Option[Long] = None) {
  val opCode: Long = listing(currLine) % 100
  private val args = (listing(currLine) / 100).toString.map(_.asDigit).reverse.toList

  private val safeAccess = (line: Int) => if (listing.isDefinedAt(line)) listing(line) else 0

  def arg(line: Int, mode: Int = 1): Long = mode match {
    case 0 => safeAccess(safeAccess(line).toInt)
    case 1 => safeAccess(line)
    case 2 => safeAccess((safeAccess(line) + relLine).toInt)
  }

  def inputArg: Long = arg(currLine + 1)

  def singleArgOp: Long = arg(currLine + 1, args.headOption.getOrElse(0))

  def twoArgOp: (Long, Long) = {
    val fst :: snd :: Nil = args.padTo(2, 0)
    (arg(currLine + 1, fst), arg(currLine + 2, snd))
  }

  def safeUpdateLine(line: Int, value: Long): IntCodeProgram = {
    val padded: Vector[Long] = if (listing.isDefinedAt(line)) listing else listing.padTo(line + 1, 0)
    copy(padded.updated(line, value))
  }
}

object IntCodeParser {

  private def parseOpCode(program: IntCodeProgram): OpCode = {
    // 1 - Addition
    // 2 - Multiplication
    // 3 - Input
    // 4 - Output
    // 5 - Jump if zero
    // 6 - Jump if non-zero
    // 7 - Compare equals
    // 8 - Compare less than
    // 9 - Adjust relative
    program.opCode match {
      case 1 | 2 =>
        val (leftArg, rightArg) = program.twoArgOp
        val assignLoc = program.arg(program.currLine + 3)
        Algebra(program.opCode, leftArg, rightArg, assignLoc.toInt)
      case 3 => Input(program.inputArg.toInt)
      case 4 => Output(program.singleArgOp)
      case 5 | 6 =>
        val (chk, jmp) = program.twoArgOp
        Jump(program.opCode, chk, jmp.toInt)
      case 7 | 8 =>
        val (left, right) = program.twoArgOp
        val assign = program.arg(program.currLine + 3)
        Compare(program.opCode, left, right, assign.toInt)
      case 9 => Relative(program.singleArgOp.toInt)
      case 99 => Halt
      case _ => throw new UnsupportedOperationException(s"Undefined OpCode ${program.opCode}")
    }
  }

  // Evaluates the given program with all the inputs given upfront.  If inputs are missing, throws error
  def evalProgram(programListing: Vector[Long], inputs: Seq[Long] = Seq()): (Vector[Long], Seq[Long]) = {
    @scala.annotation.tailrec
    def recurse(program: IntCodeProgram, inputs: Seq[Long], outputs: Seq[Long]): (Vector[Long], Seq[Long]) = {
      val (newProg, finished) = interactiveProgram(program.copy(io = inputs.headOption))
      if (finished) (newProg.listing, outputs)
      else recurse(
        newProg,
        inputs = if (inputs.nonEmpty) inputs.tail else inputs,
        outputs = if (newProg.io.nonEmpty) outputs :+ newProg.io.get else outputs
      )
    }

    recurse(IntCodeProgram(programListing, 0), inputs, Seq())
  }

  def readIntCodeFile(path: String): Vector[Long] = {
    readResourceFile(path).getLines().toSeq.head.split(',').toVector.map(_.toInt)
  }

  // Feed in a program and then continue until another IO occurs.
  // Useful when your program has already been provided an input
  def stepProgram(program: IntCodeProgram): (IntCodeProgram, Boolean) = {
    interactiveProgram(interactiveProgram(program)._1)
  }

  @scala.annotation.tailrec
  def interactiveProgram(program: IntCodeProgram): (IntCodeProgram, Boolean) = program match {
    case prog @ IntCodeProgram(listing, currLine, _, _) if currLine > listing.length - 1 =>
//      println(s"Given line $currLine was longer than the program listing")
      (prog.copy(io = None), true)
    case prog =>
      val opCode = parseOpCode(prog)
      val newProg = opCode.run(prog)
//      println(s"Ran $opCode")
      opCode match {
        case Halt => (newProg, true)
        case _: Input | _: Output => (newProg, false)
        case _ => interactiveProgram(newProg)
      }
  }

}

private[intcode] trait OpCode {
  def run(program: IntCodeProgram): IntCodeProgram = program
}

private[intcode] case class Algebra(opCode: Long, left: Long, right: Long, assign: Int) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = {
    program
      .safeUpdateLine(assign, opCode match {
        case 1 => left + right
        case 2 => left * right
      })
      .copy(currLine = program.currLine + 4)
  }
}

private[intcode] case class Input(assign: Int) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = program match {
    case IntCodeProgram(_, currLine, _, Some(input)) =>
      program.safeUpdateLine(assign, input).copy(currLine = currLine + 2, io = None)
    case _ => throw new UnsupportedOperationException(s"Input OpCode called with no given input in $program")
  }
}

private[intcode] case class Output(result: Long) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = program match {
    case IntCodeProgram(_, currLine, _, _) => program.copy(currLine = currLine + 2, io = Some(result))
  }
}

private[intcode] case class Jump(opCode: Long, chk: Long, line: Int) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = {
    program.copy(
      currLine = opCode match {
        case 5 => if (chk != 0) line else program.currLine + 3
        case 6 => if (chk == 0) line else program.currLine + 3
      }
    )
  }
}

private[intcode] case class Compare(opCode: Long, left: Long, right: Long, assign: Int) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = {
    program
      .safeUpdateLine(assign, opCode match {
        case 7 => if (left < right) 1 else 0
        case 8 => if (left == right) 1 else 0
      })
      .copy(currLine = program.currLine + 4)
  }
}

private[intcode] case class Relative(adjust: Int) extends OpCode {
  override def run(program: IntCodeProgram): IntCodeProgram = program match {
    case IntCodeProgram(_, currLine, relLine, _) => program.copy(currLine = currLine + 2, relLine = relLine + adjust)
  }
}

private[intcode] case object Halt extends OpCode
