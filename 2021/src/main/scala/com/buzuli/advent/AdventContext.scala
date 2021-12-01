package com.buzuli.advent

import com.buzuli.util.Time

import java.time.Instant
import scala.concurrent.duration.Duration

case class AdventContext(
  start: Instant = Time.now,
  end: Option[Instant] = None,
  results: List[DayResult] = Nil
) {
  def elapsed: Duration = Time.diff(start, end.getOrElse(Time.now))
}
