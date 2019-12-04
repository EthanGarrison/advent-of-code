package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.DaySpec
import org.scalatest.Inspectors

class Day02Spec extends DaySpec with Inspectors {

  private val testInput = Seq(12, 14, 1969, 100756)
  private val inputPath = "input/y2019/day02.txt"

  it should "handle part one test cases" in {
    val testData = Seq(
      Vector(1, 0, 0, 0, 99) -> Vector(2, 0, 0, 0, 99),
      Vector(2, 3, 0, 3, 99) -> Vector(2, 3, 0, 6, 99),
      Vector(2, 4, 4, 5, 99, 0) -> Vector(2, 4, 4, 5, 99, 9801),
      Vector(1, 1, 1, 4, 99, 5, 6, 0, 99) -> Vector(30, 1, 1, 4, 2, 5, 6, 0, 99)
    )

    forAll(testData) {
      case (input, result) => Day02.evalProgram(input) must be(result)
    }
  }

  it should "run day two" in {
    Day02.runAll(inputPath)
  }

}
