package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day6 extends AdventDay(6) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1RaceScore}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2RaceScore}"
  }

  lazy val puzzle1RaceScore: Int = {
    races.map({ race =>
      Range
        .inclusive(0, race.time.toInt)
        .map({ t => (race.time - t) * t })
        .filter(_ > race.distance)
        .length
    }).reduce(_ * _)
  }

  lazy val puzzle2RaceScore: Long = {
    val lowerBound = findFirstAbove(bigRace, 0, bigRace.time / 2)
    val upperBound = findLastAbove(bigRace, bigRace.time / 2, bigRace.time)

    upperBound - lowerBound
  }

  def findFirstAbove(race: Race, start: Long, end: Long): Long = {
    println(s"${start} - ${end}")

    val diff = end - start

    if (start == end) {
      val score = (race.time - end) * end
      if (race.distance < score) {
        start
      } else {
        start + 1
      }
    } else if (diff == 1) {
      val score = (race.time - end) * end
      if (race.distance > score) {
        end
      } else {
        start
      }
    } else {
      val pivot = start + ((end - start) / 2)
      val score = (race.time - pivot) * pivot
      if (race.distance < score) {
        findFirstAbove(race, start, pivot)
      } else {
        findFirstAbove(race, pivot, end)
      }
    }
  }

  def findLastAbove(race: Race, start: Long, end: Long): Long = {
    println(s"${start} - ${end}")

    val diff = end - start

    if (diff == 0) {
      val score = (race.time - start) * start
      if (race.distance < score) {
        start
      } else {
        start - 1
      }
    } else if (diff == 1) {
      val score = (race.time - start) * start
      if (race.distance > score) {
        start
      } else {
        end
      }
    } else {
      val pivot = start + ((end - start) / 2)
      val score = (race.time - pivot) * pivot
      if (race.distance < score) {
        findLastAbove(race, pivot, end)
      } else {
        findLastAbove(race, start, pivot)
      }
    }
  }

  lazy val bigRace: Race = {
    lines match {
      case List(
      s"Time:${timeStr}",
      s"Distance:${distStr}"
      ) => {
        Race(
          timeStr.split(' ').map(_.trim).filter(_.nonEmpty).toList.mkString("").toLong,
          distStr.split(' ').map(_.trim).filter(_.nonEmpty).toList.mkString("").toLong,
        )
      }
      case _ => Race(0L, 0L)
    }
  }

  lazy val races: List[Race] = {
    lines match {
      case List(
        s"Time:${timeStr}",
        s"Distance:${distStr}"
      ) => {
        timeStr.split(' ').view.map(_.trim).filter(_.nonEmpty).flatMap(_.toLongOption).zip(
          distStr.split(' ').view.map(_.trim).filter(_.nonEmpty).flatMap(_.toLongOption)
        ).map({
          case (t, d) => Race(t, d)
        }).toList
      }
      case _ => Nil
    }
  }

  case class Race(
    time: Long,
    distance: Long,
  )
}