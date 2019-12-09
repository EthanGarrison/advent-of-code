package com.ethang.adventofcode.y2019

trait ProblemFormat[Input, Output] {

  val partOneAnswer: Output
  val partTwoAnswer: Output

  def readInput(path: String): Input

  def partOne(inputData: Input): Output

  def partTwo(inputData: Input): Output

  def runAll(path: String): Unit = {
    val input = readInput(path)
    val partOneResult = partOne(input)
    assert(partOneResult == partOneAnswer, s"Part One Failed with result: $partOneResult")
    val partTwoResult = partTwo(input)
    assert(partTwoResult == partTwoAnswer, s"Part Two Failed with result: $partTwoResult")
  }

}
