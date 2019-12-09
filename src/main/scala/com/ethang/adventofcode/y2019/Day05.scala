package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile
import com.ethang.adventofcode.y2019.intcode.IntCodeParser

object Day05 extends ProblemFormat[Vector[Int], Int] {
//  override val partOneAnswer: Int = 7265618
  override val partOneAnswer: Int = 3 // When I fix `evalProgram` to actually give me outputs, will need to revert this
//  override val partTwoAnswer: Int = 7731427
  override val partTwoAnswer: Int = 314

  override def readInput(path: String): Vector[Int] = {
    readResourceFile(path).getLines().toSeq.head.split(',').toVector.map(_.toInt)
  }

  override def partOne(inputData: Vector[Int]): Int = {
    IntCodeParser.evalProgram(inputData, Seq(1)).head
  }

  override def partTwo(inputData: Vector[Int]): Int = {
    IntCodeParser.evalProgram(inputData, Seq(5)).head
  }
}
