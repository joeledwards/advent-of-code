package com.buzuli.advent

import com.buzuli.util.{FileSystem, Time}
import com.typesafe.scalalogging.LazyLogging

import java.io.File
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class PuzzleResult(
  day: AdventDay,
  number: Int,
  result: Try[String],
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
    if (puzzles.isEmpty || puzzles.exists(_.result.isFailure))
      "FAILED"
    else
      "SUCCEEDED"
  }

  def summary: String = {
    if (puzzles.isEmpty)
      "No puzzles."
    else
      puzzles.map(_.toString).mkString(" | ")
  }

  override def toString: String = {
    val durationString = Time.prettyDuration(duration)
    s"[${day.name} | ${outcome} | ${durationString}] ${summary}"
  }
}

abstract class AdventDay(
  val day: Int
) extends LazyLogging {
  def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]]

  final def execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    Time.asyncTiming {
      Future.sequence {
        val start = Time.nowNanos

        puzzles.zipWithIndex map { case (puzzle, index) =>
          val number = index + 1

          puzzle(context) transform { outcome =>
            val puzzleDuration = Time.sinceNanos(start)
            val result = Success(PuzzleResult(this, number, outcome, puzzleDuration))
            val message = s"${name} Puzzle ${number} => ${result} (${Time.prettyDuration(puzzleDuration)})"

            outcome match {
              case Success(_) => logger.info(message)
              case Failure(reason) => logger.error(message, reason)
            }

            result
          }
        }
      }
    } map { case (dayDuration, puzzleResults) =>
      DayResult(this, puzzleResults, dayDuration)
    }
  }

  def name: String = s"Day ${day}"

  override def toString: String = name

  lazy val _lines: List[String] = {
    val DayFileName = "^(?:.*[^0-9])?([0]*[0-9]+)(?:[^0-9].*)?$".r
    val allDataFiles = FileSystem.dataFiles
    val fileName = allDataFiles.map(f => (f, f.getName)).collectFirst {
      case (f, DayFileName(d)) if d.toInt == day => {
        val file = f.getCanonicalPath
        file
      }
    }
    if (fileName.isEmpty) {
      throw new Exception(s"No input found for ${name}")
    }
    fileName.map(FileSystem.readFileLines).getOrElse(Nil)
  }

  def lines: List[String] = _lines
}
