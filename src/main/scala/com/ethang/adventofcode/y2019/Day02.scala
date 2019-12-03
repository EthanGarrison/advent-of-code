package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day02 {

  private val partOneAnswer = 3101878
  private val partTwoAnswer = 8444

  private def evalOpCode(curr: Int, listing: Vector[Int]): Vector[Int] = {
    val opCode = listing(curr)
    val leftArg = listing(curr + 1)
    val rightArg = listing(curr + 2)
    val assignLoc = listing(curr + 3)

    val updatedValue = opCode match {
      case 1 => listing(leftArg) + listing(rightArg)
      case 2 => listing(leftArg) * listing(rightArg)
    }

    listing.updated(assignLoc, updatedValue)
  }

  def evalProgram(programListing: Vector[Int]): Vector[Int] = {
    @scala.annotation.tailrec
    def recurse(listing: Vector[Int], line: Int): Vector[Int] = {
      if(listing(line) == 99) listing
      else recurse(evalOpCode(line, listing), line + 4)
    }
    recurse(programListing, 0)
  }

  def partOne(opCodeList: Vector[Int]): Int = {
    evalProgram(opCodeList.updated(1, 12).updated(2, 2))(0)
  }

  def partTwo(opCodeList: Vector[Int]): Int = {
    val nounVerbList = for(n <- (0 to 99).iterator; v <- (0 to 99).iterator) yield (n, v)

    nounVerbList
      .find { case (n, v) => evalProgram(opCodeList.updated(1, n).updated(2, v))(0) == 19690720 }
      .map { case (n, v) => 100 * n + v}
      .get
  }

  def runAll(inputPath: String): Unit = {
    val inputData = readResourceFile(inputPath).mkString.split(',').toVector.map(_.toInt)
    val partOneResult = partOne(inputData)
    val partTwoResult = partTwo(inputData)
    assert(partOneResult == partOneAnswer, "Part One Failed")
    assert(partTwoResult == partTwoAnswer, "Part Two Failed")
  }

}
