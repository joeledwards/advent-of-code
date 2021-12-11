package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (increases, _) = soundings.foldLeft[(Int, Option[Int])]((0, None)) { (acc, sounding) =>
      acc match {
        case (increases, Some(lastSounding)) if sounding > lastSounding => (increases + 1, Some(sounding))
        case (increases, _) => (increases, Some(sounding))
      }
    }
    increases.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (increases, _) = soundings.foldLeft[(Int, List[Int])]((0, Nil)) { (acc, sounding) =>
      acc match {
        case (increases, a :: b :: c :: Nil) => {
          val lastCount = a + b + c
          val thisCount = sounding + a + b
          val increase = if (thisCount > lastCount) 1 else 0
          (increases + increase, sounding :: a :: b :: Nil)
        }
        case (increases, contributors) => {
          (increases, sounding :: contributors)
        }
      }
    }
    increases.toString
  }

  val soundings: List[Int] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList
  }
}
