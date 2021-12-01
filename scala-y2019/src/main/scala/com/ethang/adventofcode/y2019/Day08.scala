package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day08 extends ProblemFormat[Seq[Seq[Int]], Int] {
  type Layer = Seq[Int]

  override val partOneAnswer: Int = 2562
  override val partTwoAnswer: Int = 0 // Unfortunately, part two doesn't neatly fit in my trait for testing

  private val imgLength = 25
  private val imgHeight = 6

  override def readInput(path: String): Seq[Layer] = {
    readResourceFile(path).getLines()
      .toSeq.head
      .map(_.asDigit)
      .grouped(imgLength * imgHeight)
      .toSeq
//      .grouped(imgHeight)
//      .grouped(imgLength)
//      .map(_.toSeq).toSeq
  }

  override def partOne(inputData: Seq[Layer]): Int = {
    val leastZeros = inputData.minBy(layer => layer.count(_ == 0))

    val (oneCount, twoCount) = leastZeros.foldLeft((0, 0)) {
      case ((one, two), 1) => (one + 1, two)
      case ((one, two), 2) => (one, two + 1)
      case (acc, _) => acc
    }

    oneCount * twoCount
  }

  override def partTwo(inputData: Seq[Layer]): Int = {
    // Compare two rows by zipping and checking to see if we got non-zero
//    val height = imgHeight
//    val length = imgLength
//
//    def compareRow(acc: Seq[Int], cmp: Seq[Int]): Seq[Int] = {
//      acc.zip(cmp)
//        .map {
//          case (2, r) => r
//          case (l, _) => l
//        }
//    }
//
//    val compressed = inputData.reduce(compareRow)
//    println(compressed.length)
//    for (x <- 0 until height) {
//      for {
//        y <- 0 until length
//        idx = (x * length) + y
//      } print(compressed(idx))
//      println()
//    }
    // Answer is ZFLBY.  Only seen by looking at the image that is created
    0
  }
}
