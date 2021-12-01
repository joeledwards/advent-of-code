package com.buzuli.util

import com.typesafe.scalalogging.LazyLogging

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration

trait AsyncTask[T] {
  def execute(): Future[T]
}

object Async extends LazyLogging {
  /**
   * Delay asynchronously for the specified duration.
   *
   * @param duration the minimum delay
   *
   * @return a Future which we complete after the delay duration
   */
  def delay(duration: Duration): Future[Duration] = {
    val start = Instant.now
    val p: Promise[Duration] = Promise()

    Scheduler.default.runAfter(duration) {
      val end = Instant.now
      val duration = Duration(
        java.time.Duration.between(start, end).toMillis,
        TimeUnit.MILLISECONDS
      )
      p.success(duration)
    }

    p.future
  }

  /**
   * Process tasks with limited concurrency.
   *
   * @param concurrency the maximum concurrency (no more than this number of tasks will run at once)
   * @param tasks the tasks to execute
   *
   * @return a Future which will contain the results of the tasks if all succeed in the order they were received
   */
  def concurrently[T](
    concurrency: Int
  )(
    tasks: Seq[AsyncTask[T]]
  )(
    implicit ec: ExecutionContext
  ): Future[Seq[T]] = {
    var outstanding: Seq[(Int, AsyncTask[T])] = {
      tasks.zipWithIndex map { case (t, i) => (i, t) }
    }
    var completed: List[(Int, T)] = Nil

    def handleCompleted(result: Option[(Int, T)]): Future[Option[(Int, T)]] = completed.synchronized {
      result match {
        case None =>
        case Some((position, result)) => {
          completed ::= position -> result
        }
      }

      val nextTaskFuture = nextTask

      logger.info(s"[${result}] outstanding=${outstanding.length} completed=${completed.length}")

      nextTaskFuture
    }

    def nextTask: Future[Option[(Int, T)]] = outstanding.synchronized {
      outstanding match {
        case Nil => Future.successful(None)
        case (position, task) :: rest => {
          outstanding = rest
          Future.unit.flatMap { _ =>
            task.execute()
          } map { result =>
            Some((position, result))
          } flatMap {
            handleCompleted
          }
        }
      }
    }

    Future.sequence({
      (1 to concurrency) map { _ =>
        nextTask flatMap {
          handleCompleted
        }
      }
    }) map { _ =>
      completed.sortBy(_._1).map(_._2)
    }
  }
}
