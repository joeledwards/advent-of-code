package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    solveWithSparseMatrix(false).toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    solveWithSparseMatrix(true).toString
  }

  def solveWithSparseMatrix(includeDiagonals: Boolean): Int = {
    val matrix: mutable.Map[Coord, Int] = mutable.HashMap()

    val filter: Segment => Boolean = includeDiagonals match {
      case true => _ => true
      case false => _.diagonal
    }

    for (segment <- segments.filter(filter)) {
      for (coord <- segment.range) {
        matrix.updateWith(coord)(_.orElse(Some(0)).map(_ + 1))
      }
    }

    matrix.values.count(_ > 1)
  }

  case class Coord(x: Int, y: Int)

  case class Segment(
    segmentText: String
  ) {
    lazy val (start: Coord, end: Coord) = {
      segmentText match {
        case s"${sx},${sy} -> ${ex},${ey}" => {
          (
            Coord(sx.toInt, sy.toInt),
            Coord(ex.toInt, ey.toInt)
          )
        }
      }
    }

    override def toString: String = segmentText

    lazy val diagonal: Boolean = (start.x != end.x) && (start.y != end.y)
    lazy val range: List[Coord] = {
      val xLen = Math.abs(start.x - end.x) + 1
      val yLen = Math.abs(start.y - end.y) + 1

      val xRange: List[Int] = {
        if (start.x < end.x) {
          Range(start.x, end.x + 1).toList
        } else if (start.x > end.x) {
          Range(end.x, start.x + 1).toList.reverse
        } else {
          Range(0, yLen).toList.map(_ => start.x)
        }
      }

      val yRange: List[Int] = {
        if (start.y < end.y) {
          Range(start.y, end.y + 1).toList
        } else if (start.y > end.y) {
          Range(end.y, start.y + 1).toList.reverse
        } else {
          Range(0, xLen).toList.map(_ => start.y)
        }
      }

      xRange.zip(yRange).map { case (x, y) => Coord(x, y) }
    }
  }

  def segments: List[Segment] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Segment)
      .toList
  }

  lazy val sample: String =
    """
      |0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2
      |""".stripMargin
}
