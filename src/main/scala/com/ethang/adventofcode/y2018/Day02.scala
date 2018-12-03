package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.readResourceFile

import scala.language.implicitConversions

object Day02 {

  implicit def boolToInt(b: Boolean): Int = if(b) 1 else 0

  val mapHasCount: Int => Map[Char, Int] => Boolean = count => m => m.exists { case (_, c) => c == count }

  def convertToMap(str: String): Map[Char, Int] = {
    str.foldLeft(Map[Char, Int]()) { case (r, c) => r + (c -> (r.getOrElse(c, 0) + 1)) }
  }

  def taskOne(inputFile: String): Int = {
    val hasTwo = mapHasCount(2)
    val hasThree = mapHasCount(3)

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

}
