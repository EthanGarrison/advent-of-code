package com.ethang.adventofcode.y2019.intcode

object IntCodeParser {

  private type Program = (Vector[Int], Int, Seq[Int])

  private def evalOpCode(listing: Vector[Int], curr: Int, inputs: Seq[Int]): Program = {
    val currVal = listing(curr)
    val opCode = currVal % 100
    val args = (currVal / 100).toString.map(_.asDigit).reverse.toList

    def param(line: Int, paramMode: Int = 1): Int = paramMode match {
      case 0 => listing(listing(line))
      case 1 => listing(line)
    }

    // 1 - Addition
    // 2 - Multiplication
    // 3 - Read input
    // 4 - Write output
    // TODO: Update output to instead write to a state variable instead of println
    opCode match {
      case 1 | 2 =>
        val fst :: snd :: Nil = args.padTo(2, 0)
        val leftArg = param(curr + 1, fst)
        val rightArg = param(curr + 2, snd)
        val assignLoc = param(curr + 3)
        val updatedValue = if (opCode == 1) leftArg + rightArg else leftArg * rightArg
        (listing.updated(assignLoc, updatedValue), curr + 4, inputs)
      case 3 => (listing.updated(param(curr + 1), inputs.head), curr + 2, inputs.tail)
      case 4 =>
        val fst = args.headOption.getOrElse(0)
        println(param(curr + 1, fst))
        (listing, curr + 2, inputs)
      case _ => assert(assertion = false, s"Undefined OpCode $opCode"); ???
    }
  }

  def evalProgram(programListing: Vector[Int], inputs: Seq[Int] = Seq()): Vector[Int] = {
    @scala.annotation.tailrec
    def recurse(program: Program): Program = program match {
      case (listing, line, _) if listing(line) == 99 => program
      case (listing, line, inputs) => recurse(evalOpCode(listing, line, inputs))
    }

    recurse(programListing, 0, inputs)._1
  }

}
