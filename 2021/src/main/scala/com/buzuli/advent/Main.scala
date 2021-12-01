package com.buzuli.advent

import com.buzuli.days.days
import com.buzuli.util.Time
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Main extends App with LazyLogging {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  // Setup
  def before(): Future[AdventContext] = {
    Future.successful(AdventContext())
  }

  // Run advent days
  def run(context: AdventContext): Future[AdventContext] = {
    days.executeSerially(context) map { results =>
      context.copy(results = results)
    }
  }

  // Tear-down
  def after(context: AdventContext): Future[AdventContext] = {
    Future.successful(context.copy(end = Some(Time.now)))
  }

  Try {
    logger.info("Advent of Code 2021")

    Await.result(
      Future.unit flatMap { _ =>
        before()
      } flatMap { context =>
        run(context)
      } flatMap { context =>
        after(context)
      }
      , 5.seconds)
  } match {
    case Success(context) => {
      logger.info(s"Done. Took ${Time.prettyDuration(context.elapsed)}")
      context.results.foreach { result =>
        logger.info(s"${result}")
      }
    }
    case Failure(error) => {
      logger.error("Error in advent:", error)
    }
  }
}
