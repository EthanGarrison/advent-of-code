package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser.{readIntCodeFile, evalProgram}

object Day09 extends ProblemFormat[Vector[Long], Long] {
  override val partOneAnswer: Long = 2932210790L
  override val partTwoAnswer: Long = 0

  override def readInput(path: String): Vector[Long] = readIntCodeFile(path)

  override def partOne(inputData: Vector[Long]): Long = evalProgram(inputData, Seq(1))._2.head

  override def partTwo(inputData: Vector[Long]): Long = 0
}
