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
    ""
  }

  def puzzle1SumExtrapolatedNextValues: Long = {
    lines
      .map(_.split(" ").flatMap(_.toLongOption).toList)
      .map(extrapolateNextValue(_))
      .sum
  }

  case class Context(
    reduction: List[Long],
    prev: Option[Long],
    sum: Option[Long],
  )

  def extrapolateNextValue(values: List[Long]): Long = {
    var sum: Option[Long] = None
    var reduction: List[Long] = values
    var terminals: List[Long] = Nil

    while (!sum.contains(0L)) {
      val context = reduction.foldLeft[Context](Context(Nil, None, None))({ (context, value) =>
        context match {
          case Context(r, Some(prev), Some(s)) => {
            val diff = value - prev
            Context(diff :: r, Some(value), Some(s + diff))
          }
          case Context(r, _, _) => {
            Context(r, Some(value), Some(0L))
          }
        }
      })

      sum = context.sum
      reduction = context.reduction.reverse
      terminals = context.prev.getOrElse(0L) :: terminals
    }

    val prediction = terminals.sum

    println(s"${values} => ${prediction}")

    prediction
  }
}