package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day9 extends AdventDay(9) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  object p1 {
    def answer: String = ""
  }
  
  object p2 {
    def answer: String = ""
  }
}
