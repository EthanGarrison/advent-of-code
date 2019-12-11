package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser
import com.ethang.adventofcode.y2019.intcode.IntCodeParser.IntCodeProgram

object Day07 extends ProblemFormat[Vector[Int], Int] {
  override val partOneAnswer: Int = 206580
  override val partTwoAnswer: Int = 2299406

  override def readInput(path: String): Vector[Int] = IntCodeParser.readIntCodeFile(path)

  override def partOne(inputData: Vector[Int]): Int = {
    @scala.annotation.tailrec
    def recurse(phaseSettings: Seq[Int], acc: Int = 0): Int = {
      val (_, Seq(output)) = IntCodeParser.evalProgram(inputData, Seq(phaseSettings.head, acc))
      if(phaseSettings.tail.isEmpty) output
      else recurse(phaseSettings.tail, output)
    }

    (for(phaseSettings <- (0 to 4).permutations) yield recurse(phaseSettings)).max
  }

  override def partTwo(inputData: Vector[Int]): Int = {
    def parseWithFeedback(phaseSettings: Seq[Int]): Int = {
      @scala.annotation.tailrec
      def recurse(settings: Seq[Int], ampStateMap: Map[Int, IntCodeProgram], previous: Int): Int = {
        val amp = settings.head
        val (listing, line, result) = ampStateMap(amp)
        val (updated, nextLine, nextResult) = {
          val (inUpdated, inLine, _) = IntCodeParser.interactiveProgram(listing, line)(Some(previous))
          IntCodeParser.interactiveProgram(inUpdated, inLine)(None)
        }

        if (amp == phaseSettings.last && nextResult.isEmpty) result.get
        else {
          recurse(
            if(settings.tail.isEmpty) phaseSettings else settings.tail,
            ampStateMap.updated(amp, (updated, nextLine, nextResult)),
            nextResult.orElse(result).get
          )
        }
      }

      val initialState = phaseSettings.map(p => p -> IntCodeParser.interactiveProgram(inputData)(Some(p))).toMap
      recurse(phaseSettings, initialState, 0)
    }

    (for(phaseSettings <- (5 to 9).permutations) yield parseWithFeedback(phaseSettings)).max
  }
}
