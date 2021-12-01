package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day02 extends ProblemFormat[Vector[Long], Long] {

  override val partOneAnswer: Long = 3101878L
  override val partTwoAnswer: Long = 8444L

  override def readInput(path: String): Vector[Long] = IntCodeParser.readIntCodeFile(path)

  def partOne(opCodeList: Vector[Long]): Long = {
    IntCodeParser.evalProgram(opCodeList.updated(1, 12L).updated(2, 2L))._1.head
  }

  def partTwo(opCodeList: Vector[Long]): Long = {
    val testNounVerb = (n: Long, v: Long) => IntCodeParser.evalProgram(opCodeList.updated(1, n).updated(2, v))._1.head == 19690720
    (for (n <- (0L to 99L).iterator; v <- (0L to 99L).iterator if testNounVerb(n, v)) yield (n, v))
      .collectFirst { case (n, v) => 100 * n + v }
      .get
  }

}
