package com.buzuli.advent.days

import com.buzuli.advent.{AdventConcurrent, AdventContext, AdventDay, AdventSerial, DayResult}
import com.buzuli.util.{Async, AsyncTask}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

object Days extends LazyLogging {
  val dayList: List[AdventDay] = List(
    day1, day2, day3, day4, day5,
    day6, day7, day8, day9, day10,
    day11, day12, day13, day14, day15,
    day16, day17, day18, day19, day20,
    day21, day22, day23, day24, day25,
  )

  def execute(context: AdventContext)(implicit ec: ExecutionContext): Future[List[DayResult]] = {
    context.concurrency match {
      case AdventSerial => {
        dayList.foldLeft[Future[List[DayResult]]](
          Future.successful(Nil)
        ) { (previousDayFuture, nextDay) =>
          previousDayFuture.flatMap { outcomes =>
            logger.info(s"Mapping to day ${nextDay} ...")
            Async.delay(100.milliseconds) flatMap { _ =>
              logger.info(s"Delay completed. Executing ${nextDay} ...")
              nextDay.execute(context)
            } map { outcome =>
              logger.info(s"Store results for ${nextDay} ...")
              outcomes :+ outcome
            }
          }
        }
      }
      case AdventConcurrent(concurrency) => {
        Async.concurrently(concurrency) {
          dayList map { day =>
            new AsyncTask[DayResult] {
              override def execute(): Future[DayResult] = {
                logger.info(s"Mapping to day ${day} ...")
                Async.delay(100.milliseconds) flatMap { _ =>
                  logger.info(s"Delay completed. Executing ${day} ...")
                  day.execute(context) andThen {
                    case _ => logger.info(s"Store results for ${day} ...")
                  }
                }
              }
            }
          }
        } map { _.toList }
      }
    }
  }
}
