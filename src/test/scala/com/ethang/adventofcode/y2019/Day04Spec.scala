package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.DaySpec

import org.scalatest.Inspectors

class Day04Spec extends DaySpec with Inspectors {

  it should "handle part one example tests" in {
    forAll(Seq(111111L -> true, 223450L -> false, 123789L -> false)) {
      case (input, expected) => assertResult(expected)(Day04.partOneRuleCheck(input))
    }
  }

  it should "handle part two example tests" in {
    forAll(Seq(112233L -> true, 123444L -> false, 111122L -> true)) {
      case (input, expected) => assertResult(expected)(Day04.partTwoRuleCheck(input))
    }
  }

  it should "run day 04" in {
    Day04.runAll("183564-657474")
  }

}
