package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.readResourceFile

object Day05 {

  def letterTest(l: Char, r: Char): Boolean = Math.abs(l - r) != 32

  def filterLetterPair(str: String): String = {
    val letterArray = str.toCharArray.toVector

    @scala.annotation.tailrec
    def recurse(cArr: Vector[Char]): Vector[Char] = {
      var idx = 0
      var break = true
      var result = cArr
      while(break) {
        if(idx >= cArr.length - 1) break = false
        else {
          val l = cArr(idx)
          val r = cArr(idx + 1)
          break = letterTest(l, r)
          if(!break) result = cArr.take(idx) ++ cArr.drop(idx + 2)
        }
        idx += 1
      }

      if(cArr.length == result.length) cArr
      else recurse(result)
    }

    recurse(letterArray).mkString
  }

  def filterLetterPairAndConstant(str: String): String = {
    val result = str
      .toCharArray
      .distinct.sorted
      .collect { case filterMe if filterMe >= 'a' && filterMe <= 'z' =>
        val upper = (filterMe - 32).toChar
        val filtered = str.filter(c => c != filterMe && c != upper)
        val count = filterLetterPair(filtered)
        filterMe -> count
      }
      .minBy(_._2.length)

    result._2
  }

  def taskOne(inputFile: String): Int = {
    val inputData = readResourceFile(inputFile).getLines().mkString

    filterLetterPair(inputData).length
  }

  def taskTwo(inputFile: String): Int = {
    val inputData = readResourceFile(inputFile).getLines().mkString

    filterLetterPairAndConstant(inputData).length
  }

}
