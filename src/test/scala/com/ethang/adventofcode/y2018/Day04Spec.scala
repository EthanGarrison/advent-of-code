package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.DaySpec
import com.ethang.adventofcode.y2018.day4.Day04
import com.ethang.adventofcode.y2018.day4.Day04.DataEntry
import com.ethang.adventofcode.y2018.day4.GuardAction._
import org.scalatest.Inspectors

// TODO: Bad code lead to even worse tests.  Originally had better when I started writing, but just cutting them as the code got worse
// I blame everthing on `Day04.calculateGuardMinuteCount`.  Really messed up my groove.  Fix that and all will be good
class Day04Spec extends DaySpec with Inspectors {

  private val inputFile = "input/y2018/day04.txt"

  private val testData: Seq[DataEntry] = {
    Seq(
      ("1518-11-01 00:00", GuardShiftChange(10)),
      ("1518-11-01 00:05", GuardFallAsleep),
      ("1518-11-01 00:25", GuardWakeUp),
      ("1518-11-01 00:30", GuardFallAsleep),
      ("1518-11-01 00:55", GuardWakeUp),
      ("1518-11-01 23:58", GuardShiftChange(99)),
      ("1518-11-02 00:40", GuardFallAsleep),
      ("1518-11-02 00:50", GuardWakeUp),
      ("1518-11-03 00:05", GuardShiftChange(10)),
      ("1518-11-03 00:24", GuardFallAsleep),
      ("1518-11-03 00:29", GuardWakeUp),
      ("1518-11-04 00:02", GuardShiftChange(99)),
      ("1518-11-04 00:36", GuardFallAsleep),
      ("1518-11-04 00:46", GuardWakeUp),
      ("1518-11-05 00:03", GuardShiftChange(99)),
      ("1518-11-05 00:45", GuardFallAsleep),
      ("1518-11-05 00:55", GuardWakeUp)
    )
  }

  "Day 04" must "parse input into usable data" in {

    val inputSeq = Seq(
      "[1518-11-01 00:00] Guard #10 begins shift",
      "[1518-11-01 00:05] falls asleep",
      "[1518-11-01 00:25] wakes up",
      "[1518-11-01 00:30] falls asleep",
      "[1518-11-01 00:55] wakes up",
      "[1518-11-01 23:58] Guard #99 begins shift",
      "[1518-11-02 00:40] falls asleep",
      "[1518-11-02 00:50] wakes up",
      "[1518-11-03 00:05] Guard #10 begins shift",
      "[1518-11-03 00:24] falls asleep",
      "[1518-11-03 00:29] wakes up",
      "[1518-11-04 00:02] Guard #99 begins shift",
      "[1518-11-04 00:36] falls asleep",
      "[1518-11-04 00:46] wakes up",
      "[1518-11-05 00:03] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up",
    )

    forAll(inputSeq zip testData) {
      case (input, expected) => Day04.parseInputStr(input) must be(expected)
    }

  }

  it must "perform task one" in {
    Day04.findMaxGuardMinute(testData) must be (10 -> 24)
    Day04.taskOne(inputFile) must be (2467 -> 40)
  }

  it must "perform task two" in {
    Day04.findAnyGuardMaxMinute(testData) must be (99 -> 45)
    Day04.taskTwo(inputFile) must be (751 -> 13)
  }

}
