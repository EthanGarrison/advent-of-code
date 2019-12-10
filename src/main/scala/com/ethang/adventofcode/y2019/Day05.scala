package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day05 extends ProblemFormat[Vector[Int], Int] {
  override val partOneAnswer: Int = 7265618
  override val partTwoAnswer: Int = 7731427

  override def readInput(path: String): Vector[Int] = IntCodeParser.readIntCodeFile(path)

  override def partOne(inputData: Vector[Int]): Int = {
    IntCodeParser.evalProgram(inputData, Seq(1))._2.last
  }

  override def partTwo(inputData: Vector[Int]): Int = {
    IntCodeParser.evalProgram(inputData, Seq(5))._2.last
  }
}
