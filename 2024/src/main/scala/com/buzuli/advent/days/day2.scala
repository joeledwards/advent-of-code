package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.advent.days.day1.{similarityScore, totalDistance}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object day2 extends AdventDay(2) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${safeReports}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${gracefulSafeReports}"
  }

  sealed trait Trend
  case object Up extends Trend
  case object Down extends Trend
  case object Invalid extends Trend

  object Trend {
    def apply(a: Int, b: Int): Trend = {
      val delta = a - b
      if (delta < -3 || delta > 3)
        Invalid
      else if (delta > 0)
        Up
      else if (delta < 0)
        Down
      else
        Invalid
    }
  }

  case class Context(
    prior: Int,
    trend: Option[Trend],
  )

  def isSafeReport(report: List[Int]): Boolean = {
    report.foldLeft(Option.empty[Context])({ (context, currentValue) =>
        context match {
          case None                                       => Some(Context(prior = currentValue, None))
          case Some(c @ Context(_, Some(Invalid)))        => Some(c)
          case Some(Context(priorValue, historicalTrend)) => {
            val currentTrend = Trend(priorValue, currentValue)
            (historicalTrend, currentTrend) match {
              case (None, _)          => Some(Context(currentValue, Some(currentTrend)))
              case (Some(Up),   Up)   => Some(Context(currentValue, Some(Up)))
              case (Some(Down), Down) => Some(Context(currentValue, Some(Down)))
              case _                  => Some(Context(currentValue, Some(Invalid)))
            }
          }
        }
      })
      .flatMap(_.trend)
      .exists(_ != Invalid)
  }

  def safeReports: Long = {
    lines
      .map({ line =>
        line
          .split("\\s+")
          .toList
          .flatMap(v => Try(v.toInt).toOption)
      })
      .count(isSafeReport)
  }

  def isSafeReportWithGrace(report: List[Int]): Boolean = {
    if (isSafeReport(report)) {
      true
    } else {
      var pre: List[Int] = Nil
      var skip: List[Int] = Nil
      var post: List[Int] = report

      var foundValid = false

      while (!foundValid && (skip.nonEmpty || post.nonEmpty)) {
        val thisReport = pre ::: post
        if (isSafeReport(thisReport)) {
          val reportStr = thisReport.map(_.toString).mkString(" ")
          //println(s"=> $reportStr")
          foundValid = true
        } else {
          pre = pre ::: skip.headOption.toList
          skip = post.headOption.toList
          post = if (post.nonEmpty) post.tail else Nil
        }
      }

      foundValid
    }
  }

  def gracefulSafeReports: Long = {
    lines
      .map({ line =>
        line
          .split("\\s+")
          .toList
          .flatMap(v => Try(v.toInt).toOption)
      })
      .filter(isSafeReportWithGrace)
      .map(r => {
        //println(r.map(_.toString).mkString(" "))
        r
      })
      .size
  }
}
