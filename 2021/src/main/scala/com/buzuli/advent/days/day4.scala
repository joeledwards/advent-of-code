package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day4 extends AdventDay(4) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    val p1 = puzzle1.run()
    val p2 = puzzle2.run()

    //Future.successful(success(s"""Puzzle1[${p1}] Puzzle2[${p2}]"""))
    Future.successful(failure("Not implemented"))
  }

  object puzzle1 {
    def run(): Int = {
      val (duration, value) = Time.timing {
        1
      }

      logger.info(s"${name} Puzzle 1 => ${value} (${duration})")

      value
    }
  }

  object puzzle2 {
    def run(): Int = {
      val (duration, value) = Time.timing {
        2
      }


      logger.info(s"${name} Puzzle 2 => ${value} (${duration})")

      value
    }
  }

  object data {
  }
}
