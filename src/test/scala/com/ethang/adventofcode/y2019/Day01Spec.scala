package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.DaySpec

class Day01Spec extends DaySpec {

  private val testInput = Seq(12, 14, 1969, 100756)
  private val inputPath = "input/y2019/day01.txt"

  it should "handle part one test cases" in {
    val expected = Seq(2, 2, 654, 33583).sum
    Day01.partOne(testInput) must be (expected)
  }

  it should "handle part two test cases" in {
    val expected = Seq(2, 2, 966, 50346).sum
    Day01.partTwo(testInput) must be (expected)
  }

  it should "run day one" in {
    Day01.runAll(inputPath)
  }

}
