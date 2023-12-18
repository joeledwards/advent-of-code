package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day9 extends AdventDay(9) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1SumExtrapolatedNextValues}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2SumExtrapolatedPrevValues}"
  }

  def puzzle1SumExtrapolatedNextValues: Long = {
    lines
      .map(_.split(" ").flatMap(_.toLongOption).toList)
      .map(extrapolateNextValue(_))
      .sum
  }

  def puzzle2SumExtrapolatedPrevValues: Long = {
    lines
      .map(_.split(" ").flatMap(_.toLongOption).toList)
      .map(extrapolatePrevValue(_))
      .sum
  }

  case class Context(
    reduction: List[Long],
    first: Option[Long],
    prev: Option[Long],
    max: Option[Long],
    min: Option[Long],
  )

  def extrapolatePrevAndNextValues(values: List[Long]): (Long, Long) = {
    var min: Option[Long] = None
    var max: Option[Long] = None
    var reduction: List[Long] = values
    var starts: List[Long] = Nil
    var ends: List[Long] = Nil

    while (!(min.contains(0L) && max.contains(0L))) {
      val context = reduction.foldLeft[Context](Context(Nil, None, None, None, None))({ (context, value) =>
        context match {
          case Context(r, Some(first), Some(prev), Some(min), Some(max)) => {
            val diff = value - prev
            Context(diff :: r, Some(first), Some(value), Some(Math.max(max, diff)), Some(Math.min(min, diff)))
          }
          case Context(r, _, _, _, _) => {
            Context(r, Some(value), Some(value), Some(0L), Some(0L))
          }
        }
      }
      )

      max = context.max
      min = context.min
      reduction = context.reduction.reverse
      starts = context.first.getOrElse(0L) :: starts
      ends = context.prev.getOrElse(0L) :: ends
    }

    val prev = starts.foldLeft(0L)((acc, v) => v - acc)
    val next = ends.sum
    (prev, next)
  }

  def extrapolatePrevValue(values: List[Long]): Long = extrapolatePrevAndNextValues(values)._1

  def extrapolateNextValue(values: List[Long]): Long = extrapolatePrevAndNextValues(values)._2
}