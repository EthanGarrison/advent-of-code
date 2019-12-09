package com.ethang.adventofcode.y2019.intcode

object IntCodeParser {

  private def evalOpCode(listing: Vector[Int], curr: Int): Vector[Int] = {
    val currVal = listing(curr)
    val opCode = currVal % 100
    val args = (currVal / 100).toString.map(_.asDigit).reverse.toList

    val param = (line: Int, paramMode: Int) => paramMode match {
      case 0 => listing(listing(line))
      case 1 => listing(line)
    }

    opCode match {
      case 1 | 2 =>
        val fst :: snd :: assign :: Nil = args.padTo(2, 0).padTo(3, 1)
        val leftArg = param(curr + 1, fst)
        val rightArg = param(curr + 2, snd)
        val assignLoc = param(curr + 3, assign)
        val updatedValue = if(opCode == 1) leftArg + rightArg else leftArg * rightArg
        listing.updated(assignLoc, updatedValue)
      case 3 | 4 => ???
      case _ => assert(assertion = false, s"Undefined OpCode $opCode"); ???
    }
  }

  def evalProgram(programListing: Vector[Int]): Vector[Int] = {
    @scala.annotation.tailrec
    def recurse(listing: Vector[Int], line: Int): Vector[Int] = {
      if(listing(line) == 99) listing
      else recurse(evalOpCode(listing, line), line + 4)
    }
    recurse(programListing, 0)
  }

}
