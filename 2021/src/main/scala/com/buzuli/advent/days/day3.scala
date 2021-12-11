package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day3 extends AdventDay(3) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val bits = diagnostics.foldLeft[List[Int]]("000000000000".toList.map(_ => 0)) { (acc, sample) =>
      acc.zip(sample.toList).map {
        case (c, '0') => c - 1
        case (c, _) => c + 1
      }
    }

    val gamma = Integer.parseInt(new String(bits.map({
      case v if v > 0 => '1'
      case _ => '0'
    }).toArray), 2)

    val epsilon = Integer.parseInt(new String(bits.map({
      case v if v > 0 => '0'
      case _ => '1'
    }).toArray), 2)

    val power = gamma * epsilon
    power.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val o2 = {
      var remaining = diagnostics
      for (i <- 0 until 12) {
        if (remaining.length > 1) {
          val (a, b) = remaining.partition(_(i) == '1')
          remaining = if (a.length >= b.length) a else b
        }
      }
      Integer.parseInt(remaining.head, 2)
    }

    val co2 = {
      var remaining = diagnostics
      for (i <- 0 until 12) {
        if (remaining.length > 1) {
          val (a, b) = remaining.partition(_(i) == '0')
          remaining = if (a.length <= b.length) a else b
        }
      }
      Integer.parseInt(remaining.head, 2)
    }

    val lifeSupport = o2 * co2
    lifeSupport.toString
  }

  val diagnostics: List[String] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
  }
}
