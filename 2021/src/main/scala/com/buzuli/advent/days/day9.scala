package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object day9 extends AdventDay(9) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    basins.toList.map(b => b.height + 1).sum.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    basins.toList.map(_.size).sorted.reverse.take(3).foldLeft(1)(_*_).toString
  }

  case class Point(
    x: Int,
    y: Int,
    height: Int,
    contributors: mutable.Set[Point] = mutable.Set()
  ) {
    def resolveContributors: Set[Point] = {
      Set(this) | contributors.toSet.flatMap( (p: Point) => p.resolveContributors)
    }
    def size: Int = resolveContributors.size
  }

  lazy val basins: Set[Point] = {
    val map = mutable.Map[(Int, Int), Point]()
    val lowestPoints = mutable.Set[Point]()
    val height = grid.length
    val width = grid.head.length
    val xMax = width - 1
    val yMax = height - 1

    def fetchPoint(x: Int, y: Int): Option[Point] = {
      map.get((x, y)) orElse {
        grid(y)(x) match {
          case 9 => None
          case height => {
            val point = Point(x, y, height)
            map.put((x, y), point)
            Some(point)
          }
        }
      }
    }

    def heightAndContributors(point: Point)(x: Int, y: Int): Int = {
      fetchPoint(x, y) match {
        case None => 9
        case Some(other) => {
          if (point.height > other.height) {
            other.contributors.add(point)
          } else if (point.height < other.height) {
            point.contributors.add(other)
          }
          other.height
        }
      }
    }

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        fetchPoint(x, y) foreach { point =>
          val h = point.height

          val above = y match {
            case 0 => 9
            case _ => heightAndContributors(point)(x, y - 1)
          }

          val below = y match {
            case i if i == yMax => 9
            case _ => heightAndContributors(point)(x, y + 1)
          }

          val left = x match {
            case 0 => 9
            case _ => heightAndContributors(point)(x - 1, y)
          }

          val right = x match {
            case i if i == xMax => 9
            case _ => heightAndContributors(point)(x + 1, y)
          }

          val isLowPoint = h < above && h < below && h < left && h < right

          if (isLowPoint) {
            lowestPoints.add(point)
          }
        }
      }
    }

    lowestPoints.toSet
  }

  val grid: Array[Array[Int]] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toArray.map(_.toInt - 48))
      .toArray
  }

  lazy val sample: List[String] = {
    """
      |2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678
      |""".stripMargin.split("\n").toList
  }
}
