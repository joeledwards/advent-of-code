package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}

import scala.concurrent.{ExecutionContext, Future}

object day14 extends AdventDay(14) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    Future.successful(failure("Not implemented"))
  }
}
