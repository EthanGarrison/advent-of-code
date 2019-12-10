package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day02 extends ProblemFormat[Vector[Int], Int] {

  override val partOneAnswer = 3101878
  override val partTwoAnswer = 8444

  override def readInput(path: String): Vector[Int] = IntCodeParser.readIntCodeFile(path)

  def partOne(opCodeList: Vector[Int]): Int = {
    IntCodeParser.evalProgram(opCodeList.updated(1, 12).updated(2, 2))._1.head
  }

  def partTwo(opCodeList: Vector[Int]): Int = {
    val testNounVerb = (n: Int, v: Int) => IntCodeParser.evalProgram(opCodeList.updated(1, n).updated(2, v))._1.head == 19690720
    (for (n <- (0 to 99).iterator; v <- (0 to 99).iterator if testNounVerb(n, v)) yield (n, v))
      .collectFirst { case (n, v) => 100 * n + v }
      .get
  }

}
