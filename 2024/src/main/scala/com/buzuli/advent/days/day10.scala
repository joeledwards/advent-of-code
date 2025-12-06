package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.collections.{FlipList, FlipListNode}

import java.util
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day10 extends AdventDay(10) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution1"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution2"
  }

  case class Grid(
    min: Point,
    max: Point,
  ) {
    def contains(point: Point): Boolean = {
      if (point.x < min.x)
        false
      else if (point.x > max.x)
        false
      else if (point.y < min.y)
        false
      else if (point.y > max.y)
        false
      else
        true
    }
  }

  case class Point(x: Long, y: Long) {
    def cardinalAdjacencies(grid: Grid): List[Point] = {
      List(
        Point(x + 1, y + 1),
        Point(x + 1, y - 1),
        Point(x - 1, y + 1),
        Point(x - 1, y - 1),
      )
        .filter(grid.contains)
    }
  }

  case class Segment(location: Point, height: Long)

  val segments: List[Segment] = {
    lines
      .zipWithIndex
      .flatMap({ case (line, y) =>
        line
          .split("")
          .toList
          .map(_.toLong)
          .zipWithIndex
          .map({ case (height, x) =>
            Segment(Point(x, y), height)
          })
      })
  }

  val graph: Grid = {
    val yMax: Long = lines.length - 1
    val xMax: Long = lines.headOption.map(l => l.length - 1L).getOrElse(0L)
    Grid(Point(0, 0), Point(xMax, yMax))
  }

  def allPathsCount: Long = {
    val segmentSet: Set[Segment] = segments.toSet
    var pathCounts: Map[Point, Long] = Map.empty

    def pathCount(segment: Segment): Long = {
      segment match {
        case Segment(_, 9)     => 1
        case Segment(point, height) => {
          pathCounts.get(point) match {
            case Some(count) => count
            case None => {
              val count = segment
                .location
                .cardinalAdjacencies(graph)
                .map(p => Segment(p, height + 1))
                .filter(s => segmentSet.contains(s))
                .map(pathCount)
                .sum

              pathCounts += point -> count

              count
            }
          }
        }
      }
    }

    val trailheads = segments.filter(_.height == 0L)

    println(s"trailheads => $trailheads")

    trailheads
      .map(pathCount)
      .sum
  }

  lazy val solution1: Long = allPathsCount
  lazy val solution2: Long = 0L
}
