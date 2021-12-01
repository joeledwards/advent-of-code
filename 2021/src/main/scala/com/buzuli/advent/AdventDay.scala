package com.buzuli.advent

import com.buzuli.util.Time
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

case class DayResult(
  day: AdventDay,
  success: Boolean,
  message: String,
  duration: Option[Duration] = None
) {
  def outcome: String = success match {
    case true => "SUCCEEDED"
    case false => "FAILED"
  }

  override def toString: String = s"[${day.name} | ${outcome} | ${duration.map(Time.prettyDuration).getOrElse("--")}] ${message}"
}

abstract class AdventDay(
  val day: Int
) extends LazyLogging {
  final def execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult] = {
    val start = Time.now
    _execute(context) map { result =>
      result.copy(duration = Some(Time.since(start)))
    }
  }

  def _execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult]

  def success(message: String): DayResult = DayResult(this, true, message)
  def failure(message: String): DayResult = DayResult(this, false, message)

  def name: String = s"Day ${day}"

  override def toString: String = name
}
