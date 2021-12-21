package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.sun.org.apache.xerces.internal.impl.dv.util.HexBin

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day17 extends AdventDay(17) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    "--"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    "--"
  }

  lazy val sample = List("--")
}