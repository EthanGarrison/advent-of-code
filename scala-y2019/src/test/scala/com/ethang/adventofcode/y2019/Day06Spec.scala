package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.BaseSpec

class Day06Spec extends BaseSpec {

  import Day06._

  private val inputPath = "input/y2019/day06.txt"
  private val inputData = Map(
    "COM" -> Seq("B"),
    "B" -> Seq("G", "C"),
    "G" -> Seq("H"),
    "C" -> Seq("D"),
    "D" -> Seq("E", "I"),
    "E" -> Seq("J", "F"),
    "J" -> Seq("K"),
    "K" -> Seq("L")
  )

  it should "run part one examples" in {
    partOne(inputData) must be(42)
  }

  it should "run part two examples" in {
    val input = inputData ++ Map("K" -> Seq("L", "YOU"), "I" -> Seq("SAN"))
    partTwo(input) must be(4)
  }

  it should "run day 06" in {
    runAll(inputPath)
  }

}
