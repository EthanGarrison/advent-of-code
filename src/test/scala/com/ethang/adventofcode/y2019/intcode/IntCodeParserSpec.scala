package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.BaseSpec

class IntCodeParserSpec extends BaseSpec {

  import com.ethang.adventofcode.y2019.intcode.IntCodeParser._

  ignore should "test multiplication/addition" in {
    val testData = Seq(
      Vector(1, 0, 0, 0, 99) -> Vector(2, 0, 0, 0, 99),
      Vector(2, 3, 0, 3, 99) -> Vector(2, 3, 0, 6, 99),
      Vector(2, 4, 4, 5, 99, 0) -> Vector(2, 4, 4, 5, 99, 9801),
      Vector(1, 1, 1, 4, 99, 5, 6, 0, 99) -> Vector(30, 1, 1, 4, 2, 5, 6, 0, 99),
      Vector(1002, 4, 3, 4, 33) -> Vector(1002, 4, 3, 4, 99)
    )

    forAll(testData) { case (input, result) => evalProgram(input) must be(result) }
  }

  ignore should "test I/O" in {
    val testData = Seq(
      (Vector(3, 0, 4, 0, 99), Seq(5)) -> Vector(5, 0, 4, 0, 99)
    )

    forAll(testData) { case ((listing, inputs), result) => evalProgram(listing, inputs) must be(result) }
  }

  it should "test jumps and conditionals" in {
    println(evalProgram(Vector(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), Seq(1)))
    // Vector(3, 3, 1105, 0, 9, 1101, 0, 0, 12, 4, 12, 99, 1)
    //
  }

}
