package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.BaseSpec

class IntCodeParserSpec extends BaseSpec {

  import com.ethang.adventofcode.y2019.intcode.IntCodeParser._

  it should "handle part one test cases" in {
    val testData = Seq(
      Vector(1, 0, 0, 0, 99) -> Vector(2, 0, 0, 0, 99),
      Vector(2, 3, 0, 3, 99) -> Vector(2, 3, 0, 6, 99),
      Vector(2, 4, 4, 5, 99, 0) -> Vector(2, 4, 4, 5, 99, 9801),
      Vector(1, 1, 1, 4, 99, 5, 6, 0, 99) -> Vector(30, 1, 1, 4, 2, 5, 6, 0, 99)
    )

    forAll(testData) { case (input, result) => evalProgram(input) must be(result) }
  }

}
