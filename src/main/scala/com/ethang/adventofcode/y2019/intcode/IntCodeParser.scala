package com.ethang.adventofcode.y2019.intcode

object IntCodeParser {

  private type Program = (Vector[Int], Int, Seq[Int])

  private def evalOpCode(listing: Vector[Int], curr: Int, inputs: Seq[Int]): Program = {
    val currVal = listing(curr)
    val opCode = currVal % 100
    val args = (currVal / 100).toString.map(_.asDigit).reverse.toList
//    println(s"Given op code $opCode")

    def param(line: Int, paramMode: Int = 1): Int = paramMode match {
      case 0 => listing(listing(line))
      case 1 => listing(line)
    }

    def oneArg: Int = {
      param(curr + 1, args.headOption.getOrElse(0))
    }

    def twoArg: (Int, Int) = {
      val fst :: snd :: Nil = args.padTo(2, 0)
      (param(curr + 1, fst), param(curr + 2, snd))
    }

    // 1 - Addition
    // 2 - Multiplication
    // 3 - Read input
    // 4 - Write output
    // TODO: Update output to instead write to a state variable instead of println
    opCode match {
      case 1 | 2 =>
//        println("Running algebra")
        val (leftArg, rightArg) = twoArg
        val assignLoc = param(curr + 3)
        val updatedValue = if (opCode == 1) leftArg + rightArg else leftArg * rightArg
        (listing.updated(assignLoc, updatedValue), curr + 4, inputs)
      case 3 =>
//        println("Reading input")
        (listing.updated(param(curr + 1), inputs.head), curr + 2, inputs.tail)
      case 4 =>
        println(oneArg)
        (listing, curr + 2, inputs)
      case 5 | 6 =>
        val (chk, jmp) = twoArg
        val jmpTest = if(opCode == 5) chk != 0 else chk == 0
//        println(s"Running jmps, checking $chk with jmp to $jmp")
        (listing, if(jmpTest) jmp else curr + 3, inputs)
      case 7 | 8 =>
        val (left, right) = twoArg
        val assign = param(curr + 3)
//        println(s"Running comparison with args left: $left, right: $right, assign: $assign")
        val test = if(opCode == 7) left < right else left == right
        (listing.updated(assign, if(test) 1 else 0), curr + 4, inputs)
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
