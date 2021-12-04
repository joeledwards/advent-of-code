package com.buzuli.advent

import com.buzuli.util.{Async, Time}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class PuzzleResult(
  day: AdventDay,
  number: Int,
  result: Try[Int],
  duration: Duration
) {
  override def toString: String = {
    val outcome = result match {
      case Success(value) => s"Success(${value})"
      case Failure(reason) => s"Failure(${reason.getMessage})"
    }

    s"Puzzle ${number} => ${outcome}"
  }
}

case class DayResult(
  day: AdventDay,
  puzzles: List[PuzzleResult],
  duration: Duration
) {
  def outcome: String = {
    if (!puzzles.exists(_.result.isFailure)) "SUCCEEDED" else "FAILED"
  }

  def summary: String = puzzles.map(_.toString).mkString(" | ")

  override def toString: String = {
    val durationString = Time.prettyDuration(duration)
    s"[${day.name} | ${outcome} | ${durationString}] ${summary}"
  }
}

abstract class AdventDay(
  val day: Int
) extends LazyLogging {
  def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[Int]]

  final def execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    val start = Time.now

    Future.sequence {
      val start = Time.now

      puzzles.zipWithIndex map { case (puzzle, index) =>
        val duration = Time.since(start)
        val number = index + 1

        puzzle(context) transform { outcome =>
          val result = Success(PuzzleResult(this, number, outcome, duration))
          val message = s"${name} Puzzle ${number} => ${result} (${Time.prettyDuration(duration)})"

          outcome match {
            case Success(_) => logger.info(message)
            case Failure(reason) => logger.error(message, reason)
          }

          result
        }
      }
    } map { puzzleResults =>
      DayResult(this, puzzleResults, Time.since(start))
    }
  }

  def name: String = s"Day ${day}"

  override def toString: String = name
}
