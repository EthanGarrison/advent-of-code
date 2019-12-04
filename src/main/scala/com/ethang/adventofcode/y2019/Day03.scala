package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day03 {

  private val partOneAnswer = 1195

  sealed trait WireDirection

  type WireMap = Map[(Int, Int), Int]
  type Instruction = (WireDirection, Int)

  object InstructionOps {
    private val instrRgx = """([RLUD])([0-9]+)""".r

    def from(str: String): Instruction = str match {
      case instrRgx(dir, dist) =>
        val wireDirection = dir.head match {
          case 'R' => Right
          case 'L' => Left
          case 'U' => Up
          case 'D' => Down
        }
        (wireDirection, dist.toInt)
    }

    case object Right extends WireDirection

    case object Left extends WireDirection

    case object Up extends WireDirection

    case object Down extends WireDirection

  }

  private def updateCoord(instr: Instruction, coord: (Int, Int)): (Int, Int) = {
    val (dir, dist) = instr
    val (x, y) = coord
    dir match {
      case InstructionOps.Right => (x + dist, y)
      case InstructionOps.Left => (x - dist, y)
      case InstructionOps.Up => (x, y + dist)
      case InstructionOps.Down => (x, y - dist)
    }
  }

  private def drawWire(wireMap: WireMap, start: (Int, Int), end: (Int, Int)) = {
    val (sX, sY) = start
    val (eX, eY) = end
    val line = for {
      x <- if (sX <= eX) sX to eX else eX to sX
      y <- if (sY <= eY) sY to eY else eY to sY
    } yield (x, y) -> wireMap.getOrElse((x, y), 1) // Don't increment, as we don't want to count self-overlaps
    wireMap ++ line
  }

  @scala.annotation.tailrec
  private def drawWireMap(wire: Seq[Instruction], wireMap: WireMap, coord: (Int, Int)): WireMap = {
    if (wire.isEmpty) wireMap
    else {
      val newCoord = updateCoord(wire.head, coord)
      drawWireMap(wire.tail, drawWire(wireMap, coord, newCoord), newCoord)
    }
  }

  private def prettyWireMap(wireMap: WireMap): String = {
    val xSeq = wireMap.keys.toSeq.map(_._1)
    val ySeq = wireMap.keys.toSeq.map(_._2)

    val rows = for (x <- xSeq.min to xSeq.max) yield {
      val row = for (y <- ySeq.min to xSeq.max) yield wireMap.getOrElse((x, y), 0) match {
        case 0 => '.';
        case 1 => '#'
        case 2 => '+'
      }
      row.mkString(" ")
    }

    rows.reverse.mkString("\n")
  }

  private def mergeWireMap(left: WireMap, right: WireMap): WireMap = {
    (left.iterator ++ right.iterator)
      .to(LazyList)
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).sum)
      .toMap
  }

  private def checkIntersection(wireMap: WireMap)(x: Int, y: Int): Boolean = {
    Seq((x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)).forall { coord => wireMap.getOrElse(coord, 0) > 0 }
  }

  def inputToWires(input: Seq[String]): (Seq[Instruction], Seq[Instruction]) = {
    val strToWire = (s: String) => s.split(',').map(InstructionOps.from).toSeq
    input match {
      case Seq(fst, snd) => (strToWire(fst), strToWire(snd))
    }
  }

  def partOne(firstWire: Seq[Instruction], secondWire: Seq[Instruction]): Int = {
    val _draw = (instrList: Seq[Instruction]) => drawWireMap(instrList, Map(), (0, 0))
    val wireMap = mergeWireMap(_draw(firstWire), _draw(secondWire))
    val chkIntersect = checkIntersection(wireMap) _

    wireMap.iterator
      .collect {
        // Find intersection, which should have a point in each direction
        case ((x, y), cnt) if cnt > 1 && chkIntersect(x, y) =>
          val dist = Math.abs(x) + Math.abs(y)
          println(s"Found Intersection at ($x, $y), distance is $dist")
          dist
      }
      .toSeq.min
  }

  def runAll(inputPath: String): Unit = {
    val (fst, snd) = inputToWires(readResourceFile(inputPath).getLines().toSeq)
    val partOneResult = partOne(fst, snd)
    assert(partOneResult == partOneAnswer, "Part One failed")
    println(s"\nResult: $partOneResult\n")
  }

}
