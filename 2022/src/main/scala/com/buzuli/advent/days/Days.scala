package com.buzuli.advent.days

import com.buzuli.advent.{AdventConcurrent, AdventContext, AdventDay, AdventSerial, DayResult}
import com.buzuli.util.{Async, AsyncTask}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ExecutionContext, Future}

object Days extends LazyLogging {
  val dayList: List[AdventDay] = List(
    day1,  day2,  day3,  day4,  day5,
    day6,  day7,  day8,  day9,  //day10,
    //day11, day12, day13, day14, day15,
    //day16, day17, day18, day19, day20,
    //day21, day22, day23, day24, day25,
  )

  def execute(
    context: AdventContext, dayFilter: AdventDay => Boolean
  )(implicit ec: ExecutionContext): Future[List[DayResult]] = {
    context.concurrency match {
      case AdventSerial => {
        dayList.filter(dayFilter).foldLeft[Future[List[DayResult]]](
          Future.successful(Nil)
        ) { (previousDayFuture, nextDay) =>
          previousDayFuture.flatMap { outcomes =>
            Future.unit flatMap { _ =>
              nextDay.execute(context)
            } map { outcome =>
              outcomes :+ outcome
            }
          }
        }
      }
      case AdventConcurrent(concurrency) => {
        Async.concurrently(concurrency) {
          dayList filter { dayFilter } map { day =>
            new AsyncTask[DayResult] {
              override def execute(): Future[DayResult] = {
                Future.unit flatMap { _ =>
                  day.execute(context)
                }
              }
            }
          }
        } map { _.toList }
      }
    }
  }
}
