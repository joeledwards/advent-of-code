package com.buzuli.util

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{Duration, DurationLong}
import scala.util.Try

object Time {
  val NANOS_PER_MICRO: Long = 1000L
  val NANOS_PER_MILLI: Long = NANOS_PER_MICRO * 1000L
  val NANOS_PER_SECOND: Long = NANOS_PER_MILLI * 1000L
  val NANOS_PER_MINUTE: Long = NANOS_PER_SECOND * 60L
  val NANOS_PER_HOUR: Long = NANOS_PER_MINUTE * 60L
  val NANOS_PER_DAY: Long = NANOS_PER_HOUR * 24L

  def now: Instant = Instant.now
  def since(whence: Instant, timeSource: => Instant = now): Duration = diff(whence, timeSource)
  def diff(start: Instant, end: Instant): Duration = {
    Duration(
      java.time.Duration.between(start, end).toMillis,
      TimeUnit.MILLISECONDS
    )
  }

  def nowNanos: Long = System.nanoTime
  def sinceNanos(whence: Long, timeSource: => Long = nowNanos): Duration = diffNanos(whence, timeSource)
  def diffNanos(start: Long, end: Long): Duration = (end - start).nanos

  def thousandths(value: Number): String = s"${(value.intValue() % 1000) + 1000}".slice(1, 4)
  def prettyDuration(duration: Duration): String = duration.toNanos match {
    case nanos if nanos >= NANOS_PER_DAY  => s"${nanos / NANOS_PER_DAY}d ${nanos / NANOS_PER_HOUR % 24}h"
    case nanos if nanos >= NANOS_PER_HOUR => s"${nanos / NANOS_PER_HOUR}h ${nanos / NANOS_PER_MINUTE % 60}m"
    case nanos if nanos >= NANOS_PER_MINUTE => s"${nanos / NANOS_PER_MINUTE}m ${nanos / NANOS_PER_SECOND % 60}s"
    case nanos if nanos >= NANOS_PER_SECOND => s"${nanos / NANOS_PER_SECOND}.${thousandths(nanos / NANOS_PER_MILLI % 1000)}s"
    case nanos if nanos >= NANOS_PER_MILLI => s"${nanos / NANOS_PER_MILLI}.${thousandths(nanos / NANOS_PER_MICRO % 1000)}ms"
    case nanos => s"${nanos / NANOS_PER_MICRO}.${thousandths(nanos % 1000)}us"
  }

  def timing[T](action: => T): (Duration, T) = {
    val start = System.nanoTime
    val result: T = action
    val end = System.nanoTime
    val duration = (end - start).nanos
    (duration, result)
  }

  def safeTiming[T](action: => T): Try[(Duration, T)] = Try(timing(action))

  def asyncTiming[T](action: => Future[T])(implicit ec: ExecutionContext): Future[(Duration, T)] = {
    val start = System.nanoTime
    Future.unit flatMap { _ =>
      action
    } map { result =>
      val end = System.nanoTime
      val duration = (end - start).nanos
      (duration, result)
    }
  }
}
