package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.readResourceFile

import scala.annotation.tailrec

object Day01 {

  def readInputData(input: Seq[String]): Seq[Int] = input.map(_.toInt)

  def taskOne(inputFile: String): Int = {
    val inputData = readResourceFile(inputFile).getLines().toSeq

    readInputData(inputData).sum
  }

  def findFirstDuplicate(inputData: Seq[Int]): Int = {
    @tailrec
    def search(current: Int = 0, data: Seq[Int] = inputData, seen: Set[Int] = Set()): Int = {
      if (seen contains current) current
      else {
        search(
          current + data.head,
          if (data.tail.isEmpty) inputData else data.tail,
          seen + current
        )
      }
    }

    search()
  }

  def taskTwo(inputFile: String): Int = {
    val inputData = readInputData(readResourceFile(inputFile).getLines().toSeq)
    findFirstDuplicate(inputData)
  }

}
