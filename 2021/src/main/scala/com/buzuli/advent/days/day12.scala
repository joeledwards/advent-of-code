package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}

import scala.concurrent.{ExecutionContext, Future}

object day12 extends AdventDay(12) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[Int]] = Nil
}
