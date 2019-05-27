package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.DaySpec
import org.scalatest.Inspectors

class Day05Spec extends DaySpec with Inspectors {

  private val inputFile = "input/y2018/day05.txt"

  "Day 05" must "filter upper-lower letter pairs" in {
    Day05.filterLetterPair("dabAcCaCBAcCcaDA") must be("dabCBAcaDA")
  }

  it must "test for upper-lower pairs correctly" in {
    val input = Seq(
      ('a', 'A', false),
      ('A', 'a', false),
      ('a', 'b', true),
      ('b', 'a', true),
      ('b', 'A', true),
      ('A', 'b', true)
    )

    forAll(input) {
      case (l, r, result) => Day05.letterTest(l, r) must be(result)
    }

  }

  ignore must "perform task one" in {
    Day05.taskOne(inputFile) must be(9370)
  }

  it must "perform task two" in {
    Day05.filterLetterPairAndConstant("dabAcCaCBAcCcaDA").length must be (4)
//    Day05.taskTwo(inputFile) must be(6390)
  }

}
