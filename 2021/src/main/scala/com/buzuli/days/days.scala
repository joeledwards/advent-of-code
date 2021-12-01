package com.buzuli.days

import com.buzuli.advent.{AdventContext, AdventDay, DayResult}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ExecutionContext, Future}

object days extends LazyLogging {
  val dayList: List[AdventDay] = List(
    day1
  )

  def executeSerially(context: AdventContext)(implicit ec: ExecutionContext): Future[List[DayResult]] = {
    dayList.foldLeft[Future[List[DayResult]]](
      Future.successful(Nil)
    ) { (previousDayFuture, nextDay) =>
      previousDayFuture.flatMap { outcomes =>
        nextDay.execute(context) map { outcome =>
          outcomes :+ outcome
        }
      }
    }
  }
}
