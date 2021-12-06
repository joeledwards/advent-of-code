package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day7 extends AdventDay(7) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = Nil
}
