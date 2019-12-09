package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.BaseSpec

class Day02Spec extends BaseSpec {

  private val inputFile = "input/y2018/day02.txt"

  private val inputData = Seq(
    ("abcdef", Map('a' -> 1, 'b' -> 1, 'c' -> 1, 'd' -> 1, 'e' -> 1, 'f' -> 1)),
    ("bababc", Map('a' -> 2, 'b' -> 3, 'c' -> 1)),
    ("abbcde", Map('a' -> 1, 'b' -> 2, 'c' -> 1, 'd' -> 1, 'e' -> 1)),
    ("abcccd", Map('a' -> 1, 'b' -> 1, 'c' -> 3, 'd' -> 1)),
    ("aabcdd", Map('a' -> 2, 'b' -> 1, 'c' -> 1, 'd' -> 2)),
    ("abcdee", Map('a' -> 1, 'b' -> 1, 'c' -> 1, 'd' -> 1, 'e' -> 2)),
    ("ababab", Map('a' -> 3, 'b' -> 3))
  )

  "Day Two" must "convert string to map of character counts" in {
    inputData.foreach { case (input, expected) => Day02.convertToMap(input) must be(expected) }
  }

  it must "check map contains expected counts" in {
    val hasOne = Day02.mapHasCount(1)
    val hasTwo = Day02.mapHasCount(2)
    val hasThree = Day02.mapHasCount(3)

    val expected = Seq(
      (true, false, false),
      (true, true, true),
      (true, true, false),
      (true, false, true),
      (true, true, false),
      (true, true, false),
      (false, false, true)
    )

    inputData
      .map(_._2)
      .map { d => (hasOne(d), hasTwo(d), hasThree(d)) }
      .zip(expected)
      .foreach { case (i, e) => i must be(e) }
  }

  it must "score two strings differences" in {
    val expected = Seq(2, 5, 1, 5, 5, 5)
    Seq("abcde", "axcye", "fghij", "fguij", "klmno", "pqrst", "wvxyz")
      .sliding(2).toSeq
      .zip(expected)
      .foreach {
        case (Seq(l, r), e) => Day02.scoreStringComparison(l, r) must be(e)
      }
  }

  it must "perform task one" in {
    Day02.taskOne(inputFile) must be(9139)
  }

  it must "perform task two" in {
    Day02.taskTwo(inputFile) must be("uqcidadzwtnhsljvxyobmkfyr")
  }
}
