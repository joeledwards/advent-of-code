package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}

import scala.concurrent.{ExecutionContext, Future}

object day9 extends AdventDay(9) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    Future.successful(failure("Not implemented"))
  }
}
