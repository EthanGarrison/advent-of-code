package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.readResourceFile

object IntCodeParser {

  private type Program = (Vector[Int], Int, Seq[Int], Seq[Int])

  private def evalOpCode(program: Program): Program = {
    val (listing, curr, input, output) = program
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
        (listing.updated(assignLoc, updatedValue), curr + 4, input, output)
      case 3 =>
//        println(s"Reading input $oneArg")
        (listing.updated(param(curr + 1), input.head), curr + 2, input.tail, output)
      case 4 =>
//        println(s"Writing output $oneArg")
        (listing, curr + 2, input, output :+ oneArg)
      case 5 | 6 =>
        val (chk, jmp) = twoArg
        val jmpTest = if (opCode == 5) chk != 0 else chk == 0
//        println(s"Running jmps, checking $chk with jmp to $jmp")
        (listing, if (jmpTest) jmp else curr + 3, input, output)
      case 7 | 8 =>
        val (left, right) = twoArg
        val assign = param(curr + 3)
        //        println(s"Running comparison with args left: $left, right: $right, assign: $assign")
        val test = if (opCode == 7) left < right else left == right
        (listing.updated(assign, if (test) 1 else 0), curr + 4, input, output)
      case _ => throw new UnsupportedOperationException(s"Undefined OpCode $opCode")
    }
  }

  def evalProgram(programListing: Vector[Int], inputs: Seq[Int] = Seq()): (Vector[Int], Seq[Int]) = {
    @scala.annotation.tailrec
    def recurse(program: Program): Program = program match {
      case (listing, line, _, _) if listing(line) == 99 => program
      case (listing, line, input, output) => recurse(evalOpCode(listing, line, input, output))
    }

    val (listing, _, _, output) = recurse(programListing, 0, inputs, Seq())
    (listing, output)
  }

  def readIntCodeFile(path: String): Vector[Int] = readResourceFile(path).getLines().toSeq.head.split(',').toVector.map(_.toInt)

}
