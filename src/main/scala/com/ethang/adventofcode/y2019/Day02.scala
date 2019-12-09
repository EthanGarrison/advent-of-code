package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser
import com.ethang.adventofcode.readResourceFile

object Day02 extends ProblemFormat[Vector[Int], Int] {

  val partOneAnswer = 3101878
  val partTwoAnswer = 8444

  override def readInput(path: String): Vector[Int] = readResourceFile(path).mkString.split(',').toVector.map(_.toInt)

  def partOne(opCodeList: Vector[Int]): Int = {
    IntCodeParser.evalProgram(opCodeList.updated(1, 12).updated(2, 2))(0)
  }

  def partTwo(opCodeList: Vector[Int]): Int = {
    val testNounVerb = (n: Int, v: Int) => IntCodeParser.evalProgram(opCodeList.updated(1, n).updated(2, v))(0) == 19690720
    (for (n <- (0 to 99).iterator; v <- (0 to 99).iterator if testNounVerb(n, v)) yield (n, v))
      .collectFirst { case (n, v) => 100 * n + v }
      .get
  }

}
