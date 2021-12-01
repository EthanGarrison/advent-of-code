package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.BaseSpec

class Day01Spec extends BaseSpec {

  private val inputData = "input/y2018/day01.txt"

  "Day One" must "parse input string to int seq" in {
    val expected = Seq(1, -2, 3, 1)

    val inputStr = Seq("1", "-2", "3", "1")

    val result = Day01.readInputData(inputStr)

    result must not be empty
    result must be(expected)
  }

  it must "find first duplicate" in {
    Seq(
      (0, Seq(1, -1)),
      (2, Seq(1, -2, 3, 1)),
      (5, Seq(-6, 3, 8, 5, -6)),
      (10, Seq(3, 3, 4, -2, -4)),
      (14, Seq(7, 7, -2, -7, -4)),
    )
      .foreach { case (expected, input) => Day01.findFirstDuplicate(input) must be(expected) }

    Day01.findFirstDuplicate(Seq(1, -2, 3, 1)) must be(2)
    Day01.findFirstDuplicate(Seq(1, -1)) must be(0)
    Day01.findFirstDuplicate(Seq(3, 3, 4, -2, -4)) must be(10)
    Day01.findFirstDuplicate(Seq(-6, 3, 8, 5, -6)) must be(5)
    Day01.findFirstDuplicate(Seq(7, 7, -2, -7, -4)) must be(14)
  }

  it must "perform task one" in {
    Day01.taskOne(inputData) must be(505)
  }

  it must "perform task two" in {
    Day01.taskTwo(inputData) must be (72330)
  }

}
