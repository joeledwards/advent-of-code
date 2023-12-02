package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day2 extends AdventDay(2) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$puzzle1ValidGames"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$puzzle2GamePowerSums"
  }

  def puzzle1ValidGames: Int = {
    games
      .filter({ game =>
        !game.samples.exists({ sample =>
          sample.get(Blue).exists(_ > 14) ||
            sample.get(Green).exists(_ > 13) ||
            sample.get(Red).exists(_ > 12)
        })
      })
      .map(_.id)
      .sum
  }

  def puzzle2GamePowerSums: Int = {
    games
      .map({ game =>
        val maxBlue = game.samples.flatMap(_.get(Blue)).max
        val maxGreen = game.samples.flatMap(_.get(Green)).max
        val maxRed = game.samples.flatMap(_.get(Red)).max

        maxBlue * maxGreen * maxRed
      })
      .sum
  }

  def games: List[Game] = {
    lines
      .map({ case s"Game ${id}: ${samples}" =>
        Game(
          id = id.toInt,
          samples = {
            samples.split(";").view.map(_.trim).toList.map({ sample =>
              sample.split(",").view.map(_.trim).toList.map({
                case s"${count} blue" => Blue -> count.toInt
                case s"${count} green" => Green -> count.toInt
                case s"${count} red" => Red -> count.toInt
              }).toMap
            })
          }
        )
      })
  }

  case class Game(
    id: Int,
    samples: List[Map[Color, Int]],
  )

  sealed trait Color
  case object Blue extends Color
  case object Green extends Color
  case object Red extends Color
}
