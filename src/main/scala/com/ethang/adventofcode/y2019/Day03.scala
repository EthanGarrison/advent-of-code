package com.ethang.adventofcode.y2019

import com.ethang.adventofcode.readResourceFile

object Day03 {

  private val partOneAnswer = 1195
  private val partTwoAnswer = 91518

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

  private def genWireSnippet(instr: Instruction, coord: (Int, Int)): Seq[(Int, Int)] = {
    val (dir, dist) = instr
    val (x, y) = coord
    dir match {
      case InstructionOps.Right => for(n <- x to (x + dist)) yield (n, y)
      case InstructionOps.Left => for(n <- x to (x - dist) by -1) yield (n, y)
      case InstructionOps.Up => for(n <- y to (y + dist)) yield (x, n)
      case InstructionOps.Down => for(n <- y to (y - dist) by -1) yield (x, n)
    }
  }

  @scala.annotation.tailrec
  private def drawWireMap(wire: Seq[Instruction], wireMap: WireMap, coord: (Int, Int)): WireMap = {
    if (wire.isEmpty) wireMap
    else {
      val wireSnip = genWireSnippet(wire.head, coord)
      val updatedMap = wireMap ++ wireSnip.map(coord => coord -> wireMap.getOrElse(coord, 1))
      drawWireMap(wire.tail, updatedMap, wireSnip.last)
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
        case ((x, y), cnt) if cnt > 1 && chkIntersect(x, y) => Math.abs(x) + Math.abs(y)
      }
      .toSeq.min
  }

  private def countStepToIntersection(intersection: (Int, Int))(wire: Seq[Instruction]): Int = {
    @scala.annotation.tailrec
    def recurse(w: Seq[Instruction], coord: (Int, Int) = (0, 0), steps: Int = 0): Int = {
      if(w.isEmpty) steps
      else {
        val instr @ (dir, dist) = w.head
        val wireSnip = genWireSnippet(instr, coord)
        val indexOf = wireSnip.indexOf(intersection)
        if(indexOf < 0) recurse(w.tail, wireSnip.last, steps + dist)
        else {
          steps + (dir match {
            case InstructionOps.Right | InstructionOps.Up => dist - indexOf
            case InstructionOps.Left | InstructionOps.Down => indexOf
          })
        }
      }
    }

    recurse(wire)
  }

  def partTwo(firstWire: Seq[Instruction], secondWire: Seq[Instruction]): Int = {
    val _draw = (instrList: Seq[Instruction]) => drawWireMap(instrList, Map(), (0, 0))
    val wireMap = mergeWireMap(_draw(firstWire), _draw(secondWire))
    val chkIntersect = checkIntersection(wireMap) _

    wireMap.iterator
      .collect {
        // Find intersection, which should have a point in each direction
        case ((x, y), cnt) if cnt > 1 && chkIntersect(x, y) => (x, y)
      }
      .map { intersection =>
        val countStep = countStepToIntersection(intersection) _
        val fstCount = countStep(firstWire)
        val sndCount = countStep(secondWire)
        fstCount + sndCount
      }
      .min
  }

  def runAll(inputPath: String): Unit = {
    val (fst, snd) = inputToWires(readResourceFile(inputPath).getLines().toSeq)
    val partOneResult = partOne(fst, snd)
    val partTwoResult = partTwo(fst, snd)
    assert(partOneResult == partOneAnswer, "Part One Failed")
    assert(partTwoResult == partTwoAnswer, "Part Two Failed")
  }

}
