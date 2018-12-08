package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.DaySpec
import com.ethang.adventofcode.y2018.Day03.{Point, Rectangle}

class Day03Spec extends DaySpec {

  private val inputData = "input/y2018/day03.txt"

  "Day Three" must "transform input string into usable data" in {
    Seq(
      ("#123 @ 3,2: 5x4", Rectangle(123, Point(3, 2), Point(7, 5))),
      ("#1 @ 1,3: 4x4", Rectangle(1, Point(1, 3), Point(4, 6))),
      ("#2 @ 3,1: 4x4", Rectangle(2, Point(3, 1), Point(6, 4))),
      ("#3 @ 5,5: 2x2", Rectangle(3, Point(5, 5), Point(6, 6)))
    )
      .foreach {
        case (input, expected) => Day03.parseInputStr(input) must be(expected)
      }
  }

  it must "build collision map of given rectangle" in {
    val rect1 = Rectangle(1, Point(1, 3), Point(4, 6))
    val rect2 = Rectangle(2, Point(3, 1), Point(6, 4))
    val rect3 = Rectangle(3, Point(5, 5), Point(6, 6))

    Seq(
      (rect1, Map(Point(2, 5) -> 1, Point(1, 5) -> 1, Point(3, 4) -> 1, Point(4, 4) -> 1, Point(1, 6) -> 1, Point(3, 6) -> 1, Point(3, 5) -> 1, Point(4, 6) -> 1, Point(4, 5) -> 1, Point(1, 4) -> 1, Point(2, 6) -> 1, Point(1, 3) -> 1, Point(2, 4) -> 1, Point(3, 3) -> 1, Point(2, 3) -> 1, Point(4, 3) -> 1)),
      (rect2, Map(Point(5, 2) -> 1, Point(5, 1) -> 1, Point(3, 4) -> 1, Point(6, 4) -> 1, Point(3, 1) -> 1, Point(6, 1) -> 1, Point(4, 1) -> 1, Point(6, 2) -> 1, Point(4, 4) -> 1, Point(6, 3) -> 1, Point(5, 4) -> 1, Point(3, 2) -> 1, Point(4, 2) -> 1, Point(5, 3) -> 1, Point(3, 3) -> 1, Point(4, 3) -> 1)),
      (rect3, Map(Point(5, 5) -> 1, Point(5, 6) -> 1, Point(6, 5) -> 1, Point(6, 6) -> 1))
    )
      .foreach { case (input, expected) => Day03.rectangleToCollisionMap(input) must be(expected) }

  }

  it must "perform task one" in {
    Day03.taskOne(inputData) must be(110827)
  }

  it must "validate point is in rectangle" in {
    val rect = Rectangle(123, Point(3, 2), Point(7, 5))
    Seq(
      (Point(0, 0), false),
      (Point(1, 8), false),
      (Point(3, 2), true),
      (Point(5, 5), true)
    )
      .foreach { case (input, expected) => Day03.rectContainsPoint(rect, input) must be(expected) }
  }

  it must "perform task two" in {
    Day03.taskTwo(inputData) must be(116)
  }
}
