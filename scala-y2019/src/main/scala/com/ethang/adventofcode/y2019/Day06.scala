package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day06 extends ProblemFormat[Map[String, Seq[String]], Int] {
  val partOneAnswer: Int = 204521
  val partTwoAnswer: Int = 307

  private val ROOT_ORBIT = "COM" // All values are orbiting relative to this
  private val inputRgx = """(.*?)\)(.*)""".r

  override def readInput(path: String): Map[String, Seq[String]] = {
    readResourceFile(path)
      .getLines()
      .collect { case inputRgx(l, r) => (l, r) }
      .to(LazyList)
      .groupBy(_._1)
      .view.mapValues(_.map(_._2)).toMap
  }

  override def partOne(inputData: Map[String, Seq[String]]): Int = {
    def recurse(obj: String, depth: Int = 0): Int = {
      val orbits = inputData.getOrElse(obj, Seq())
      val children = if (orbits.isEmpty) 0 else (for (orbit <- orbits) yield recurse(orbit, depth + 1)).sum
      depth + children
    }

    recurse(ROOT_ORBIT)
  }

  override def partTwo(inputData: Map[String, Seq[String]]): Int = {
    def pathToChild(orbit: String, child: String): Seq[String] = {
      if (orbit == child) Seq(child)
      else {
        val childOrbits = inputData.getOrElse(orbit, Seq())
        if (childOrbits.isEmpty) Seq()
        else {
          val subPath = inputData.getOrElse(orbit, Seq()).to(LazyList)
            .map(pathToChild(_, child))
            .find(_.contains(child))
            .getOrElse(Seq())
          orbit +: subPath
        }
      }
    }

    val pathToMe = pathToChild(ROOT_ORBIT, "YOU").init
    val pathToSanta = pathToChild(ROOT_ORBIT, "SAN").init
//    println(s"Path to Me was $pathToMe")
//    println(s"Path to Santa was $pathToSanta")

    val prefixLength = pathToMe.zipAll(pathToSanta, "", "").takeWhile { case (l, r) => l == r }.length
    pathToMe.length + pathToSanta.length - (2 * prefixLength)
  }
}
