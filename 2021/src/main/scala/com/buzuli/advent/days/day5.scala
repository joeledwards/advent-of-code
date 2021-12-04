package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day5 extends AdventDay(5) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    val r1 = puzzle1
    val r2 = puzzle2
    for {
      p1 <- r1
      p2 <- r2
    } yield {
      success(s"""Puzzle1 => ${p1} | Puzzle2 => ${p2}""")
      failure("Not Implemented")
    }
  }

  def puzzle1(implicit ec: ExecutionContext): Future[Int] = Future {
    val (duration, value) = Time.timing {
      1
    }

    logger.info(s"${name} Puzzle 1 => ${value} (${Time.prettyDuration(duration)})")

    value
  }

  def puzzle2(implicit ec: ExecutionContext): Future[Int] = Future {
    val (duration, value) = Time.timing {
      2
    }

    logger.info(s"${name} Puzzle 1 => ${value} (${Time.prettyDuration(duration)})")

    value
  }

  object data {

    val raw: String =
      """
        |
        |""".stripMargin
  }
}
