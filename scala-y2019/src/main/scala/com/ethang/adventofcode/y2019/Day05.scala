package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day05 extends ProblemFormat[Vector[Long], Long] {
  override val partOneAnswer: Long = 7265618L
  override val partTwoAnswer: Long = 7731427L

  override def readInput(path: String): Vector[Long] = IntCodeParser.readIntCodeFile(path)

  override def partOne(inputData: Vector[Long]): Long = {
    IntCodeParser.evalProgram(inputData, Seq(1))._2.last
  }

  override def partTwo(inputData: Vector[Long]): Long = {
    IntCodeParser.evalProgram(inputData, Seq(5))._2.last
  }
}
