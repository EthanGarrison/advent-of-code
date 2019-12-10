package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day07 extends ProblemFormat[Vector[Int], Int] {
  override val partOneAnswer: Int = 206580
  override val partTwoAnswer: Int = 0

  override def readInput(path: String): Vector[Int] = IntCodeParser.readIntCodeFile(path)

  override def partOne(inputData: Vector[Int]): Int = {
    @scala.annotation.tailrec
    def recurse(phaseSettings: Seq[Int], acc: Int = 0): Int = {
      val (_, Seq(output)) = IntCodeParser.evalProgram(inputData, Seq(phaseSettings.head, acc))
      if(phaseSettings.tail.isEmpty) output
      else recurse(phaseSettings.tail, output)
    }

    (for(phaseSettings <- (0 to 4).permutations) yield recurse(phaseSettings)).max
  }

  override def partTwo(inputData: Vector[Int]): Int = 0
}
