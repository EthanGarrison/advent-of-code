package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.readResourceFile

import scala.language.implicitConversions

object Day02 {

  val mapHasCount: Int => Map[Char, Int] => Boolean = count => m => m.exists { case (_, c) => c == count }

  def convertToMap(str: String): Map[Char, Int] = {
    str.foldLeft(Map[Char, Int]()) { case (r, c) => r + (c -> (r.getOrElse(c, 0) + 1)) }
  }

  def scoreStringComparison(left: String, right: String): Int = {
    val lengthDiff = left.length - right.length

    val charDiff = left
      .zip(right)
      .map { case (l, r) => if (l != r) 1 else 0 }
      .sum

    lengthDiff + charDiff
  }

  def taskOne(inputFile: String): Int = {
    val hasTwo = mapHasCount(2)
    val hasThree = mapHasCount(3)

    implicit def boolToInt(b: Boolean): Int = if (b) 1 else 0

    val counts = readResourceFile(inputFile)
      .getLines()
      .toSeq
      .map(convertToMap)
      .map { d => (hasTwo(d), hasThree(d)) }
      .foldLeft((0, 0)) {
        case ((twoCount, threeCount), (hasTwoCount, hasThreeCount)) =>
          (twoCount + hasTwoCount, threeCount + hasThreeCount)
      }

    counts._1 * counts._2
  }

  def taskTwo(inputFile: String): String = {
    readResourceFile(inputFile)
      .getLines()
      .toSeq.sorted
      .sliding(2)
      .find { case Seq(l, r) => scoreStringComparison(l, r) == 1 }
      .map {
        case Seq(l, r) => l
          .zip(r)
          .filter { case (lc, rc) => lc == rc }
          .map(_._1)
          .mkString
      }
      .getOrElse("")
  }

}
