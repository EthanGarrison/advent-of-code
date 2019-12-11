package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.readResourceFile

object IntCodeParser {

  type IntCodeProgram = (Vector[Int], Int, Option[Int])

  private def parseOpCode(programListing: Vector[Int], current: Int, input: Option[Int]): OpCode = {
    val currVal = programListing(current)
    val opCode = currVal % 100
    val args = (currVal / 100).toString.map(_.asDigit).reverse.toList
//    println(s"Given op code $opCode")

    def param(line: Int, paramMode: Int = 1): Int = paramMode match {
      case 0 => programListing(programListing(line))
      case 1 => programListing(line)
    }

    def oneArg: Int = {
      param(current + 1, args.headOption.getOrElse(0))
    }

    def twoArg: (Int, Int) = {
      val fst :: snd :: Nil = args.padTo(2, 0)
      (param(current + 1, fst), param(current + 2, snd))
    }

    // 1 - Addition
    // 2 - Multiplication
    // 5 - Jump if zero
    // 6 - Jump if non-zero
    // 7 - Compare equals
    // 8 - Compare less than
    opCode match {
      case 1 | 2 =>
        val (leftArg, rightArg) = twoArg
        val assignLoc = param(current + 3)
        Algebra(opCode, leftArg, rightArg, assignLoc)
      case 3 => input match {
        case Some(i) => Input(i, param(current + 1))
        case None => throw new UnsupportedOperationException("Attempted to read input, but none was given")
      }
      case 4 => Output(oneArg)
      case 5 | 6 =>
        val (chk, jmp) = twoArg
//        println(s"Running jmps, checking $chk with jmp to $jmp")
        Jump(opCode, chk, jmp)
      case 7 | 8 =>
        val (left, right) = twoArg
        val assign = param(current + 3)
//        println(s"Running comparison with args left: $left, right: $right, assign: $assign")
        Compare(opCode, left, right, assign)
      case 99 => Halt
      case _ => throw new UnsupportedOperationException(s"Undefined OpCode $opCode")
    }
  }

  // Evaluates the given program with all the inputs given upfront.  If inputs are missing, throws error
  def evalProgram(programListing: Vector[Int], inputs: Seq[Int] = Seq()): (Vector[Int], Seq[Int]) = {
    @scala.annotation.tailrec
    def recurse(listing: Vector[Int], line: Int, inputs: Seq[Int], outputs: Seq[Int]): (Vector[Int], Seq[Int]) = {
      val (updated, nextLine, output) = interactiveProgram(listing, line)(inputs.headOption)
      if(line == nextLine) (updated, outputs)
      else recurse(
        listing = updated,
        line = nextLine,
        inputs = if(inputs.nonEmpty) inputs.tail else inputs,
        outputs = if(output.nonEmpty) outputs :+ output.get else outputs
      )
    }

    recurse(programListing, 0, inputs, Seq())
  }

  def readIntCodeFile(path: String): Vector[Int] = {
    readResourceFile(path).getLines().toSeq.head.split(',').toVector.map(_.toInt)
  }

  def interactiveProgram(programListing: Vector[Int], currentLine: Int = 0): Option[Int] => IntCodeProgram = input => {
    @scala.annotation.tailrec
    def recurse(listing: Vector[Int], line: Int): IntCodeProgram = {
      if (line > listing.length - 1) (listing, line, None)
      else {
        val opCode = parseOpCode(listing, line, input)
        val (newListing, newLine) = opCode.run(listing, line)
        opCode match {
          case Halt | Input(_, _) =>
//            println(s"Ran $opCode, old line was $line new line is $newLine")
            (newListing, newLine, None)
          case Output(result) =>
//            println(s"Ran output opcode")
            (newListing, newLine, Some(result))
          case _ =>
//            println(s"Ran $opCode, next up is line $newLine in $newListing")
            recurse(newListing, newLine)
        }
      }
    }
    recurse(programListing, currentLine)
  }

}

private[intcode] trait OpCode {
  def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) = (program, currentLine)
}

private[intcode] case class Algebra(opCode: Int, left: Int, right: Int, assign: Int) extends OpCode {
  override def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) = (
    program.updated(assign, opCode match {
      case 1 => left + right
      case 2 => left * right
    }),
    currentLine + 4
  )
}

private[intcode] case class Input(input: Int, assign: Int) extends OpCode {
  override def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) =
    (program.updated(assign, input), currentLine + 2)
}

private[intcode] case class Output(result: Int) extends OpCode {
  override def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) = (program, currentLine + 2)
}

private[intcode] case class Jump(opCode: Int, chk: Int, line: Int) extends OpCode {
  override def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) = (
    program,
    opCode match {
      case 5 => if (chk != 0) line else currentLine + 3
      case 6 => if (chk == 0) line else currentLine + 3
    }
  )
}

private[intcode] case class Compare(opCode: Int, left: Int, right: Int, assign: Int) extends OpCode {
  override def run(program: Vector[Int], currentLine: Int): (Vector[Int], Int) = (
    program.updated(assign, opCode match {
      case 7 => if (left < right) 1 else 0
      case 8 => if (left == right) 1 else 0
    }),
    currentLine + 4
  )
}

private[intcode] case object Halt extends OpCode