package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

object day3 extends AdventDay(3) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$allSums"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$redLightGreenLight"
  }

  def sumInstructions(text: String): Long = {
    text
      .split("mul\\(")
      .tail
      .toList
      .flatMap(_.split("\\)").toList.headOption)
      .flatMap(v => {
        v.split(",").toList match {
          case a :: b :: Nil => Try(a.toLong * b.toLong).toOption
          case _ => None
        }
      })
      .sum
  }

  def allSums: Long = sumInstructions(lines.mkString("\n"))

  def redLightGreenLight: Long = {
    lines
      .mkString("\n")
      .split("do\\(\\)")
      .toList
      .flatMap(_.split("don't\\(\\)").toList.headOption)
      .map(sumInstructions)
      .sum
  }
}
