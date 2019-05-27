package com.ethang.adventofcode.y2018.day4

import com.ethang.adventofcode.readResourceFile
import com.ethang.adventofcode.y2018.day4.GuardAction._

import scala.collection.mutable

object Day04 {

  type DataEntry = (String, GuardAction)

  def parseInputStr(str: String): DataEntry = {
    // Can't figure out/really hate working with regex, so going with substring
    (str.substring(1, 17), GuardAction.stringToAction(str.substring(19)))
  }

  // TODO: Oof.  Yeah, I just went with the dirty way of doing this.  Should probably clean someday...
  def calculateGuardMinuteCount(guardData: Seq[DataEntry]): Map[Int, Map[Int, Long]] = {
    val sortedData = guardData.sortBy(_._1)

    var currentGuard: Int = -1

    val guardState = mutable.Map[Int, String]()

    val guardMinuteSleepCount = mutable.Map[Int, mutable.Map[Int, Long]]()

    for ((time, action) <- sortedData) {
      action match {
        case GuardShiftChange(guardId) => currentGuard = guardId
        case GuardFallAsleep =>
          guardState.update(currentGuard, time)
        case GuardWakeUp =>
          guardState
            .get(currentGuard)
            .foreach { sleepTimestamp =>
              // According to the problem, the guards only sleep during a single hour, so only the minutes count
              val timeFellAsleep = sleepTimestamp.takeRight(2).toInt
              // The minute they wake up does not count, so bump down one
              val timeWokeUp = time.takeRight(2).toInt - 1

              val minuteCount = guardMinuteSleepCount.getOrElseUpdate(currentGuard, mutable.Map())

              (timeFellAsleep to timeWokeUp).foreach { minute =>
                minuteCount.update(minute, minuteCount.getOrElseUpdate(minute, 0) + 1)
              }
            }
      }
    }

    guardMinuteSleepCount.mapValues(_.toMap).toMap
  }

  def findMaxGuardMinute(guardData: Seq[DataEntry]): (Int, Int) = {
    // First, we want to sort by the timestamp given, so that we can figure shift changes and sleep/wakes
    val guardMinuteSleepCount = calculateGuardMinuteCount(guardData)

    val maxGuard = guardMinuteSleepCount.mapValues(_.values.sum).toSeq.maxBy(_._2)._1
    val maxMinute = guardMinuteSleepCount.get(maxGuard).map(_.toSeq.maxBy(_._2)._1).getOrElse(0)

    (maxGuard, maxMinute)
  }

  // TODO: Aaaaand another bad one.  The issue is just that if I sprinkled in some pattern matching, might be readable
  def findAnyGuardMaxMinute(guardData: Seq[DataEntry]): (Int, Int) = {
    val guardMinuteCount = calculateGuardMinuteCount(guardData)

    val guardMaxMinute = guardMinuteCount.mapValues(_.maxBy(_._2))

    val result = guardMaxMinute.maxBy(_._2._2)

    (result._1, result._2._1)
  }

  def taskOne(inputFile: String): (Int, Int) = {
    val inputData = readResourceFile(inputFile).getLines()

    val guardData = inputData.map(parseInputStr)

    findMaxGuardMinute(guardData.toSeq)
  }

  def taskTwo(inputFile: String): (Int, Int) = {
    val inputData = readResourceFile(inputFile).getLines()

    val guardData = inputData.map(parseInputStr)

    findAnyGuardMaxMinute(guardData.toSeq)
  }

}
