package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.y2019.intcode.IntCodeParser.interactiveProgram
import com.ethang.adventofcode.y2019.intcode.{IntCodeParser, IntCodeProgram}

object Day07 extends ProblemFormat[Vector[Long], Long] {
  override val partOneAnswer: Long = 206580L
  override val partTwoAnswer: Long = 2299406L

  override def readInput(path: String): Vector[Long] = IntCodeParser.readIntCodeFile(path)

  override def partOne(inputData: Vector[Long]): Long = {
    @scala.annotation.tailrec
    def recurse(phaseSettings: Seq[Int], acc: Long = 0): Long = {
      val (_, Seq(output)) = IntCodeParser.evalProgram(inputData, Seq(phaseSettings.head, acc))
      if (phaseSettings.tail.isEmpty) output
      else recurse(phaseSettings.tail, output)
    }

    (for (phaseSettings <- (0 to 4).permutations) yield recurse(phaseSettings)).max
  }

  override def partTwo(inputData: Vector[Long]): Long = {
    def parseWithFeedback(phaseSettings: Seq[Int]): Long = {

      @scala.annotation.tailrec
      def recurse(settings: Seq[Int], ampStateMap: Map[Int, IntCodeProgram], previous: Long = 0): Long = {
        val amp = settings.head
        val ampProg = ampStateMap(amp)
        val (updatedProg, finished) = IntCodeParser.stepProgram(ampProg.copy(io = Some(previous)))

        if (amp == phaseSettings.last && finished) ampProg.io.get
        else {
          recurse(
            if (settings.tail.isEmpty) phaseSettings else settings.tail,
            ampStateMap.updated(amp, updatedProg),
            updatedProg.io.orElse(ampProg.io).get
          )
        }
      }

      recurse(
        phaseSettings,
        phaseSettings
          .map(p => p -> interactiveProgram(IntCodeProgram(inputData, 0, io = Some(p)))._1)
          .toMap
      )
    }

    (for (phaseSettings <- (5 to 9).permutations) yield parseWithFeedback(phaseSettings)).max
  }
}
