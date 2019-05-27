package com.ethang.adventofcode.y2018.day4

sealed trait GuardAction

object GuardAction {

  case object GuardWakeUp extends GuardAction

  case object GuardFallAsleep extends GuardAction

  case class GuardShiftChange(guardId: Int) extends GuardAction

  def stringToAction(str: String): GuardAction = {
    val shiftChangeRgx = """Guard #([0-9]+) begins shift""".r

    str match {
      case "falls asleep" => GuardFallAsleep
      case "wakes up" => GuardWakeUp
      case shiftChangeRgx(guardId) => GuardShiftChange(guardId.toInt)
    }
  }

}