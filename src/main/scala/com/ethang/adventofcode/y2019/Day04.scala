package com.ethang.adventofcode.y2019

object Day04 extends ProblemFormat[(Long, Long), Int] {

  override val partOneAnswer = 1610
  override val partTwoAnswer = 1104

  private val checkLength = (s: Seq[Char]) => s.length == 6
  private val checkAscending = (s: Seq[Char]) => s.sorted == s

  def partOneRuleCheck(i: Long): Boolean = {
    val s = i.toString.toSeq
    val checkDoubleDigit = s.sliding(2).exists { case Seq(l, r) => l == r }
    checkLength(s) && checkAscending(s) && checkDoubleDigit
  }

  def partTwoRuleCheck(i: Long): Boolean = {
    val s = i.toString.toSeq
    val checkDoubleDigit = (' ' +: s :+ ' ').sliding(4).exists {
      case Seq(ol, l, r, or) => (ol != l) && (l == r) && (r != or)
    }

    checkLength(s) && checkAscending(s) && checkDoubleDigit
  }

  override def readInput(path: String): (Long, Long) = {
    val Array(lower, upper) = path.split('-').map(_.toLong)
    (lower, upper)
  }

  override def partOne(inputData: (Long, Long)): Int = inputData match {
    case (lower, upper) => (lower to upper).count(partOneRuleCheck)
  }

  override def partTwo(inputData: (Long, Long)): Int = inputData match {
    case (lower, upper) => (lower to upper).count(partTwoRuleCheck)
  }
}
