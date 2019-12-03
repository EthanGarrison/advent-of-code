package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day02 {

  private val partOneAnswer = 3101878

  private def evalOpCode(curr: Int, listing: Map[Int, Int]): Map[Int, Int] = {
//    println(s"Listing given: \n${listing.mkString("\n")}")
    val opCode = listing(curr)
    val leftArg = listing(curr + 1)
    val rightArg = listing(curr + 2)
    val assignLoc = listing(curr + 3)
//    println(s"Evaluating op code $opCode: $assignLoc = $leftArg ${if(opCode == 1) "+" else "*"} $rightArg")

    val updatedValue = opCode match {
      case 1 => listing(leftArg) + listing(rightArg)
      case 2 => listing(leftArg) * listing(rightArg)
    }

    listing + (assignLoc -> updatedValue)
  }

  def evalProgram(programListing: Map[Int, Int]): Map[Int, Int] = {
    @scala.annotation.tailrec
    def recurse(listing: Map[Int, Int], line: Int): Map[Int, Int] = {
      if(listing(line) == 99) listing
      else recurse(evalOpCode(line, listing), line + 4)
    }
    recurse(programListing, 0)
  }

  def partOne(opCodeList: Seq[Int]): Int = {
    evalProgram(opCodeList.zipWithIndex.map(_.swap).toMap ++ Map(1 -> 12, 2 -> 2))(0)
  }

  def partTwo(opCodeList: Seq[Int]): Int = {
    val programListing = opCodeList.zipWithIndex.map(_.swap).toMap

    val nounVerbList = for(n <- (0 to 99).iterator; v <- (0 to 99).iterator) yield (n, v)

    nounVerbList
      .find { case (n, v) => evalProgram(programListing ++ Map(1 -> n, 2 -> v))(0) == 19690720 }
      .map { case (n, v) => 100 * n + v}
      .get
  }

  def runAll(inputPath: String): Unit = {
    val inputData = readResourceFile(inputPath).mkString.split(',').toSeq.map(_.toInt)
    val partOneResult = partOne(inputData)
    val partTwoResult = partTwo(inputData)
    assert(partOneResult == partOneAnswer, "Part One Failed")
    println(s"\nResult for part two: $partTwoResult\n")
//    println(s"\nResult for part one: \n${partOneResult.toSeq.sortBy(_._1).mkString("\n")}\n")
  }

}
