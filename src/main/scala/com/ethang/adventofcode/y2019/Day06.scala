package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day06 extends ProblemFormat[Map[String, Seq[String]], Int] {
  val partOneAnswer: Int = 204521
  val partTwoAnswer: Int = 307

  private val ROOT_ORBIT = "COM" // All values are orbiting relative to this
  private val inputRgx = """(.*?)\)(.*)""".r

  private case class Tree[D](root: D, children: LazyList[Tree[D]]) {
    def getPathToChild(child: D): Seq[D] = {
      if(root == child) Seq(child)
      else root +: children.map(t => t.getPathToChild(child)).find(_.contains(child)).getOrElse(Seq())
    }
  }

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
//      println(s"For $obj, found orbits $orbits")
      val children = if (orbits.isEmpty) 0 else (for (orbit <- orbits) yield recurse(orbit, depth + 1)).sum
      depth + children
    }
    recurse(ROOT_ORBIT)
  }

  override def partTwo(inputData: Map[String, Seq[String]]): Int = {
    def buildTree(root: String): Tree[String] = Tree(root, inputData.getOrElse(root, Seq()).to(LazyList).map(buildTree))

    val tree = buildTree(ROOT_ORBIT)
    val pathToMe = tree.getPathToChild("YOU").init
    val pathToSanta = tree.getPathToChild("SAN").init
    println(s"Path to Me was $pathToMe")
    println(s"Path to Santa was $pathToSanta")

    val prefixLength = pathToMe.zipAll(pathToSanta, "", "").takeWhile { case (l, r) => l == r }.length
    pathToMe.length + pathToSanta.length - (2 * prefixLength)
  }
}
