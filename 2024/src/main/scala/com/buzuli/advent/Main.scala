package com.buzuli.advent

import com.buzuli.advent.days.Days
import com.buzuli.util.{Scheduler, Time}
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Main extends App with LazyLogging {
  val runMode: RunMode = RunOne(4)

  val concurrency: AdventConcurrency = AdventSerial
  val cec: CustomExecutionContext = ExecutionContexts.executor(8)
  implicit val ec: ExecutionContext = cec.ec

  // Setup
  def before(): Future[AdventContext] = {
    Future.successful(AdventContext(concurrency = concurrency))
  }

  sealed trait RunMode

  case object RunAll extends RunMode
  case class RunOne(day: Int) extends RunMode
  case class RunOnly(days: Set[Int]) extends RunMode
  case class RunExcept(days: Set[Int]) extends RunMode

  // Run advent days
  def run(context: AdventContext): Future[AdventContext] = {
    val dayFilter: AdventDay => Boolean = { adventDay =>
      runMode match {
        case RunAll => true
        case RunOne(dayNumber) => adventDay.day == dayNumber
        case RunOnly(daysToRun) => daysToRun.contains(adventDay.day)
        case RunExcept(daysToSkip) => !daysToSkip.contains(adventDay.day)
      }
    }

    Days.execute(context, dayFilter) map { results =>
      context.copy(results = results)
    }
  }

  // Tear-down
  def after(context: AdventContext): Future[AdventContext] = {
    Future.successful(context.copy(end = Some(Time.now)))
  }

  Try {
    logger.info("Advent of Code 2024")

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
      1.hour
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

  // We will hang otherwise.
  cec.halt()
}
