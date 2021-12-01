package com.buzuli.advent

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.{ExecutionContext, Future}

case class DayResult(
  day: AdventDay,
  success: Boolean,
  message: String
) {
  def outcome: String = success match {
    case true => "SUCCEEDED"
    case false => "FAILED"
  }

  override def toString: String = s"[${day.name} | ${outcome}] ${message}"
}

abstract class AdventDay(
  val day: Int
) extends LazyLogging {
  def execute(context: AdventContext)(implicit ec: ExecutionContext): Future[DayResult]

  def success(message: String): DayResult = DayResult(this, true, message)
  def failure(message: String): DayResult = DayResult(this, false, message)

  def name: String = s"Day ${day}"
}
