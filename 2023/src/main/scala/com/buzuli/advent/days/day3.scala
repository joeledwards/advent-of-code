package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day3 extends AdventDay(3) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1SymbolAdjacentPartNumbersSum}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2GearRatioSums}"
  }

  def puzzle1SymbolAdjacentPartNumbersSum: Long = {
    // Set method comparing adjacent points to symbol points
    val setSymbolResult = numbers
      .filter(n => n.adjacentPoints.exists(symbolPoints.contains))
      .map(_.value)
      .sum

    // Set method comparing points to symbol adjacent points
    val setAdjacentResult = numbers
      .filter(n => n.containedPoints.exists(symbolAdjacentPoints.contains))
      .map(_.value)
      .sum

    // Brute-force method
    val bruteForceResult = numbers
        .filter(n => {
          symbols.exists(symbolPoint =>
            n.containedPoints.exists(numberPoint =>
              symbolPoint.location.isAdjacent(numberPoint)
            )
          )
        })
        .map(_.value)
        .sum

    setSymbolResult
  }

  def puzzle2GearRatioSums: Long = {
    // This approach returns too few gear ratios.
    //
    // I was hoping this could be used to optimize the linking of gears to adjacent numbers,
    // but no such luck.
    val ratiosFromMap = gears
      .map(gear =>
        gear.adjacentPoints.toList.flatMap({ point =>
          pointToNumberMap.get(point).toList
        })
      )
      .map({
        case List(a, b) => a.value * b.value
        case _ => 0L
      })
      .sum

    // This approach returns the correct number of gear ratios.
    val ratiosFromRaw = gears
      .map({ gear =>
        numbers.filter(_.isNear(gear.location))
      })
      .map({
        case List(a, b) => a.value * b.value
        case _ => 0L
      })
      .sum

    ratiosFromRaw
  }

  lazy val mapElements: List[MapElement] = {
    lines
      .zipWithIndex
      .flatMap({ case (line, yInv) =>
        // Flip y-axis
        val y = lines.length - 1 - yInv
        parseLine(line, y)
      })
  }

  lazy val mapElementsSet: Set[MapElement] = mapElements.toSet

  def parseLine(line: String, y: Int): List[MapElement] = {
    var buffer: Option[(String, Point)] = None

    val collected: List[MapElement] = line
      .zipWithIndex
      .flatMap({
        case ('.', _) => {
          val b = buffer.map({ case (w, p) => NumericWord(w, p) }).toList
          buffer = None
          b
        }
        case (c, x) if c.isDigit => {
          buffer = {
            buffer match {
              case None => Some((s"$c", Point(x, y)))
              case Some((word, point)) => Some(s"$word$c", point)
            }
          }
          Nil
        }
        case (c, x) => {
          val b = buffer.map({ case (w, p) => NumericWord(w, p) }).toList
          buffer = None
          b ::: List(Symbol(c, Point(x, y)))
        }
      }
      ).toList

    val r = collected ::: buffer.map({ case (w, p) => NumericWord(w, p) }).toList
    //println(s"$line => $r")

    r
  }

  lazy val numbers: List[NumericWord] = mapElements collect { case e: NumericWord => e }
  lazy val symbols: List[Symbol] = mapElements collect { case e: Symbol => e }
  lazy val gears: List[Symbol] = mapElements collect { case g @ Symbol('*', _) => g }

  lazy val symbolPoints: Set[Point] = symbols.map(_.location).toSet
  lazy val symbolAdjacentPoints: Set[Point] = symbols.flatMap(_.adjacentPoints).toSet

  lazy val pointToNumberMap: Map[Point, NumericWord] = {
    numbers
      .flatMap(number =>
        number.containedPoints.map(_ -> number)
      )
      .toMap
  }

  case class Point(x: Int, y: Int) {
    lazy val adjacentPoints: Set[Point] = Set(
      Point(this.x, this.y - 1),
      Point(this.x, this.y + 1),
      Point(this.x - 1, this.y),
      Point(this.x + 1, this.y),
      Point(this.x - 1, this.y - 1),
      Point(this.x - 1, this.y + 1),
      Point(this.x + 1, this.y - 1),
      Point(this.x + 1, this.y + 1),
    )

    def isAdjacent(other: Point): Boolean = adjacentPoints.contains(other)
  }

  sealed trait MapElement

  case class Symbol(symbol: Char, location: Point) extends MapElement {
    def adjacentPoints: Set[Point] = location.adjacentPoints
  }

  case class NumericWord(text: String, location: Point) extends MapElement {
    val containedPoints: Set[Point] = {
      (0 until text.length)
        .map(xOffset => Point(location.x + xOffset, location.y))
        .toSet
    }

    val adjacentPoints: Set[Point] = {
      containedPoints.flatMap(_.adjacentPoints) -- containedPoints
    }

    val value: Long = text.toLong

    def isNear(location: Point): Boolean = adjacentPoints.contains(location)
  }
}