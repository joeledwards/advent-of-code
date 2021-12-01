package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}

import scala.concurrent.{ExecutionContext, Future}

object day17 extends AdventDay(17) {
  override def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    Future.successful(failure("Not implemented"))
  }
}
