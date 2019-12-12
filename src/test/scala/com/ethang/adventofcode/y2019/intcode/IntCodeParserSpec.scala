package com.ethang.adventofcode.y2019.intcode

import com.ethang.adventofcode.BaseSpec

class IntCodeParserSpec extends BaseSpec {

  import com.ethang.adventofcode.y2019.intcode.IntCodeParser._

  it should "test multiplication/addition" in {
    val testData = Seq(
      Vector[Long](1, 0, 0, 0, 99) -> Vector[Long](2, 0, 0, 0, 99),
      Vector[Long](2, 3, 0, 3, 99) -> Vector[Long](2, 3, 0, 6, 99),
      Vector[Long](2, 4, 4, 5, 99, 0) -> Vector[Long](2, 4, 4, 5, 99, 9801),
      Vector[Long](1, 1, 1, 4, 99, 5, 6, 0, 99) -> Vector[Long](30, 1, 1, 4, 2, 5, 6, 0, 99),
      Vector[Long](1002, 4, 3, 4, 33) -> Vector[Long](1002, 4, 3, 4, 99)
    )

    forAll(testData) { case (input, result) => evalProgram(input)._1 must be(result) }
  }

  it should "test I/O" in {
    val testData = Seq(
      (Vector[Long](3, 0, 4, 0, 99), Seq(5L)) -> Seq(5L),
      (Vector[Long](3, 0, 1002, 0, 3, 0, 4, 0, 99), Seq(5L)) -> Seq(15L)
    )

    forAll(testData) { case ((listing, inputs), result) => evalProgram(listing, inputs)._2 must be(result) }
  }

  it should "test jumps and conditionals" in {
    val testData = Seq(
      (Vector[Long](3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), Seq(8L)) -> Seq(1L),
      (Vector[Long](3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), Seq(9L)) -> Seq(0L),
      (Vector[Long](3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), Seq(8L)) -> Seq(0L),
      (Vector[Long](3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), Seq(5L)) -> Seq(1L),
      (Vector[Long](3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), Seq(1L)) -> Seq(1L)
    )

    forAll(testData) { case ((listing, inputs), result) => evalProgram(listing, inputs)._2 must be(result) }
  }

  it should "test relative positions" in {
    val testData = Seq(
      (Vector(109L, 1L, 204L, -1L, 1001L, 100L, 1L, 100L, 1008L, 100L, 16L, 101L, 1006L, 101L, 0L, 99L), Seq()) ->
        Seq(109L, 1L, 204L, -1L, 1001L, 100L, 1L, 100L, 1008L, 100L, 16L, 101L, 1006L, 101L, 0L, 99L),
      (Vector(1102L, 34915192L, 34915192L, 7L, 4L, 7L, 99L), Seq()) -> Seq(1219070632396864L),
      (Vector(104L, 1125899906842624L, 99L), Seq()) -> Seq(1125899906842624L)
    )

    forAll(testData) { case ((listing, inputs), result) => evalProgram(listing, inputs)._2 must be(result) }
  }

}
