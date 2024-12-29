package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day8 extends AdventDay(8) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution1"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution2"
  }

  case class Offset(
    x: Int,
    y: Int,
  )

  case class Point(
    x: Int,
    y: Int,
  ) {
    def offset(other: Point): Offset = {
      Offset(other.x - x, other.y - y)
    }

    def +(offset: Offset): Point = {
      Point(x + offset.x, y + offset.y)
    }

    def -(offset: Offset): Point = {
      Point(x - offset.x, y - offset.y)
    }
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

  case class Antenna(
    location: Point,
    frequency: String,
  )

  case class Antinode(
    location: Point,
    frequency: String,
  )

  lazy val (grid, antennas) = {
    val yMax = lines.length - 1
    val xMax = lines.headOption.map(_.length - 1).getOrElse(0)

    val antennaList = lines
      .zipWithIndex
      .flatMap({ case (line, yCoord) =>
        val y = yMax - yCoord
        line
          .split("")
          .toList
          .zipWithIndex
          .flatMap({
            case (".", _)       => Nil
            case (frequency, x) => List(Antenna(Point(x, y), frequency))
          })
      })

    (Grid(Point(0, 0), Point(xMax, yMax)), antennaList)
  }

  lazy val antennasByLocation = {
    antennas
      .map(a => a.location -> a)
      .toMap
  }

  lazy val antennasByFrequency: Map[String, List[Antenna]] = {
    antennas
      .foldLeft(Map.empty[String, List[Antenna]])({ (map: Map[String, List[Antenna]], antenna: Antenna) =>
        map.get(antenna.frequency) match {
          case None           => map + (antenna.frequency -> List(antenna))
          case Some(antennas) => map + (antenna.frequency -> (antenna :: antennas))
        }
      })
  }

  def findAntinodesForAntennaPair(
    grid: Grid,
    allowRepeats: Boolean = false,
  )(a: Antenna, b: Antenna): List[Antinode] = {
    val offset = a.location.offset(b.location)

    val antinodes = {
      if (allowRepeats) {
        val points = a.location :: Iterator
          .unfold(a.location)({ (point: Point) =>
            val candidate = point + offset
            if (grid.contains(candidate)) {
              Some(candidate -> candidate)
            } else {
              None
            }
          }).toList ::: Iterator
          .unfold(a.location)({ (point: Point) =>
            val candidate = point - offset
            if (grid.contains(candidate)) {
              Some(candidate -> candidate)
            } else {
              None
            }
          }).toList

        points.map(p => Antinode(p, a.frequency))
      } else {
        List(a.location - offset, b.location + offset)
          .filter(grid.contains)
          .map(p => Antinode(p, a.frequency))
      }
    }

    antinodes
  }

  def findAntinodes(
    antennas: List[Antenna],
    allowRepeats: Boolean = false
  ): List[Antinode] = antennas match {
    case Nil       => Nil
    case _ :: Nil  => Nil
    case a :: rest => {
      rest.flatMap(
        findAntinodesForAntennaPair(
          grid, allowRepeats = allowRepeats
        )(a, _)
      ) ::: findAntinodes(
        rest, allowRepeats = allowRepeats
      )
    }
  }

  lazy val antinodesWithoutRepeats: List[Antinode] = {
    antennasByFrequency
      .valuesIterator
      .flatMap(a => findAntinodes(a, allowRepeats = false))
      .toList
  }

  lazy val antinodesWithRepeats: List[Antinode] = {
    antennasByFrequency
      .valuesIterator
      .flatMap(a => findAntinodes(a, allowRepeats = true))
      .toList
  }

  lazy val solution1: Long = antinodesWithoutRepeats.map(_.location).toSet.size
  lazy val solution2: Long = antinodesWithRepeats.map(_.location).toSet.size
}
