package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[Int]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[Int] = Future {
    throw new Exception("Not Implemented")
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[Int] = Future {
    throw new Exception("Not Implemented")
  }

  object data {

    val raw: String =
      """
        |
        |""".stripMargin
  }
}
