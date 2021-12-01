package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.readResourceFile

case class IntCodeProgram(listing: Vector[Long], currLine: Int, relLine: Int = 0, io: Option[Long] = None) {
  val opCode: Long = listing(currLine) % 100
  private val args = (listing(currLine) / 100).toString.map(_.asDigit).reverse.toList

  private val safeAccess = (line: Int) => if (listing.isDefinedAt(line)) listing(line) else 0
  private val argMode = (pos: Int) => if (args.isDefinedAt(pos - 1)) args(pos - 1) else 0
  private val arg = (pos: Int) => {
    val line = currLine + pos
    argMode(pos) match {
      case 0 => safeAccess(line).toInt
      case 1 => line
      case 2 => safeAccess(line).toInt + relLine
    }
  }

  def safeUpdateLine(line: Int, value: Long): IntCodeProgram = {
    val padded: Vector[Long] = if (listing.isDefinedAt(line)) listing else listing.padTo(line + 1, 0)
    copy(padded.updated(line, value))
  }

  def parseOpCode: OpCode = {
    // 1 - Addition
    // 2 - Multiplication
    // 3 - Input
    // 4 - Output
    // 5 - Jump if zero
    // 6 - Jump if non-zero
    // 7 - Compare equals
    // 8 - Compare less than
    // 9 - Adjust relative
    opCode match {
      case 1 | 2 =>
        Algebra(opCode, safeAccess(arg(1)), safeAccess(arg(2)), arg(3))
      case 3 => Input(arg(1))
      case 4 => Output(safeAccess(arg(1)))
      case 5 | 6 => Jump(opCode, safeAccess(arg(1)), safeAccess(arg(2)).toInt)
      case 7 | 8 => Compare(opCode, safeAccess(arg(1)), safeAccess(arg(2)), arg(3))
      case 9 => Relative(safeAccess(arg(1)).toInt)
      case 99 => Halt
      case _ => throw new UnsupportedOperationException(s"Undefined OpCode $opCode")
    }
  }
}

object IntCodeParser {

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
      println(s"Given line $currLine was longer than the program listing")
      (prog.copy(io = None), true)
    case prog =>
      val opCode = prog.parseOpCode
      val newProg = opCode.run(prog)
//      println(s"Ran $opCode, program: $newProg")
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
    val updated = program
      .safeUpdateLine(assign, opCode match {
        case 1 => left + right
        case 2 => left * right
      })
      .copy(currLine = program.currLine + 4)
//    println(s"Ran op code $opCode, updated value is ${updated.listing(assign)}")
    updated
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
