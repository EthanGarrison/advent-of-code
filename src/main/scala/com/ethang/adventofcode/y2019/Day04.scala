package com.ethang.adventofcode.y2019

object Day04 {

  private val partOneAnswer = 1610
  private val checkLength = (s: Seq[Char]) => s.length == 6
  private val checkAscending = (s: Seq[Char]) => s.sorted == s

  def partOneRuleCheck(i: Long): Boolean = {
    val s = i.toString.toSeq
    val checkDoubleDigit = s.sliding(2).exists { case Seq(l, r) => l == r }
    checkLength(s) && checkAscending(s) && checkDoubleDigit
  }

  def partTwoRuleCheck(i: Long): Boolean = {
    val s = i.toString.toSeq
    val checkDoubleDigit = (' ' +: s :+ ' ') .sliding(4).exists {
      case Seq(ol, l, r, or) => (ol != l) && (l == r) && (r != or)
    }

    checkLength(s) && checkAscending(s) && checkDoubleDigit
  }

  def runAll(inputStr: String): Unit = {
    val Array(lower, upper) = inputStr.split('-').map(_.toLong)

    val partOneResult = (lower to upper).count(partOneRuleCheck)
    val partTwoResult = (lower to upper).count(partTwoRuleCheck)
    assert(partOneResult == partOneAnswer, "Part One Failed")
    println(s"Result: $partTwoResult")
  }

}
