package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}

import scala.concurrent.{ExecutionContext, Future}

object day6 extends AdventDay(6) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    Future.successful(failure("Not implemented"))
  }
}
