package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day01 {

  private val partOneAnswer = 3301059
  private val partTwoAnswer = 4948732

  private def calcFuel(i: Int): Int = (i / 3) - 2

  def partOne(inputList: Seq[Int]): Int = inputList.map(calcFuel).sum

  def partTwo(inputList: Seq[Int]): Int = {

    @scala.annotation.tailrec
    def recurse(acc: Int, i: Int): Int = {
      val fuel = calcFuel(i)
      if (fuel <= 0) acc
      else recurse(acc + fuel, fuel)
    }

    inputList.foldLeft(0)(recurse)
  }

  def runAll(inputPath: String): Unit = {
    val inputData = readResourceFile(inputPath).getLines().to(LazyList).map(_.toInt)
    val partOneResult = partOne(inputData)
    val partTwoResult = partTwo(inputData)
    assert(partOneResult == partOneResult, s"Part One $partOneResult was not $partOneAnswer")
    assert(partTwoResult == partTwoResult, s"Part Two $partTwoResult was not $partTwoAnswer")
  }

}
