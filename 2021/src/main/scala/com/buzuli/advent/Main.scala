package com.buzuli.advent

import com.buzuli.advent.days.Days
import com.buzuli.util.{Scheduler, Time}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Main extends App with LazyLogging {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  //val concurrency: AdventConcurrency = AdventSerial
  val concurrency: AdventConcurrency = AdventConcurrent(8)

  // Setup
  def before(): Future[AdventContext] = {
    Future.successful(AdventContext(concurrency = concurrency))
  }

  // Run advent days
  def run(context: AdventContext): Future[AdventContext] = {
    Days.execute(context) map { results =>
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
      {
        Future.unit flatMap { _ =>
          before()
        } flatMap { context =>
          run(context)
        } flatMap { context =>
          after(context)
        } andThen { _ =>
          Scheduler.shutdown()
        }
      },
      5.seconds
    )
  } match {
    case Success(context) => {
      context.results.foreach { result => logger.info(s"${result}") }
      logger.info(s"Done. Took ${Time.prettyDuration(context.elapsed)}")
    }
    case Failure(error) => {
      logger.error("Error in advent:", error)
    }
  }
}
