package com.ethang.adventofcode.y2018

import com.ethang.adventofcode.readResourceFile

object Day03 {

  type CollisionMap = Map[Point, Int]

  case class Point(x: Int, y: Int) {
    def +(point: Point): Point = Point(this.x + point.x, this.y + point.y)

    def -(point: Point): Point = Point(this.x - point.x, this.y - point.y)
  }

  case class Rectangle(id: Int, topLeft: Point, bottomRight: Point)

  private lazy val emptySquare: CollisionMap = rectangleToCollisionMap(
    Rectangle(-1, Point(0, 0), Point(1000, 1000))
  ).mapValues(_ => 0)

  def parseInputStr(input: String): Rectangle = {
    val pointReg: String => String = sep => s"""[0-9]+?$sep[0-9]+?"""
    val reg = s"""^#([0-9]+?) @ (${pointReg(",")}): (${pointReg("x")})""".r

    val strToPoint: String => String => Point = sep => str => str.split(sep) match {
      case Array(l, r) => Point(l.toInt, r.toInt)
    }


    input match {
      case reg(id, topLeftStr, bottomRightStr) =>
        val topLeft = strToPoint(",")(topLeftStr)
        // Add the dimensions to the topLeft to get the bottom right.
        // Subtract one from each dim to adjust for starting point
        val bottomRight = topLeft + strToPoint("x")(bottomRightStr) - Point(1, 1)
        Rectangle(id.toInt, topLeft, bottomRight)
    }
  }

  def rectangleToCollisionMap(rect: Rectangle): CollisionMap = {
    val pointList = for {
      x <- rect.topLeft.x to rect.bottomRight.x
      y <- rect.topLeft.y to rect.bottomRight.y
    } yield Point(x, y)

    pointList.map(_ -> 1).toMap
  }

  def rectContainsPoint(rectangle: Rectangle, point: Point): Boolean = {
    val isInYRange = point.y >= rectangle.topLeft.y && point.y <= rectangle.bottomRight.y
    val isInXRange = point.x >= rectangle.topLeft.x && point.x <= rectangle.bottomRight.x

    isInXRange && isInYRange
  }

  def mergeCollisionMap(l: CollisionMap, r: CollisionMap): CollisionMap = {
    l ++ r.map { case (k, v) => k -> (l.getOrElse(k, 0) + v) }
  }

  def taskOne(inputFile: String): Int = {
    readResourceFile(inputFile)
      .getLines()
      .map(parseInputStr)
      .map(rectangleToCollisionMap)
      .foldLeft(emptySquare) { case (r, m) => mergeCollisionMap(r, m) }
      .values
      .count(_ > 1)
  }

  def taskTwo(inputFile: String): Int = {
    val rectangleList = readResourceFile(inputFile)
      .getLines()
      .map(parseInputStr)
      .toSeq

    val scoredPoints = rectangleList
      .map(rectangleToCollisionMap)
      .foldLeft(emptySquare) { case (r, m) => mergeCollisionMap(r, m) }
      .filter(_._2 == 1)

    rectangleList
      .map(r => (r.id, rectangleToCollisionMap(r).keys))
      .find {
        case (_, pointList) => pointList.forall(p => scoredPoints.getOrElse(p, 0) == 1)
      }
      .map(_._1)
      .getOrElse(-1)
  }

}
