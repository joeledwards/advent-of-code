package com.buzuli.advent

import com.buzuli.util.Time

import java.time.Instant
import scala.concurrent.duration.Duration

sealed trait AdventConcurrency {
  def concurrency: Int
}
case object AdventSerial extends AdventConcurrency {
  val concurrency = 1
}
case class AdventConcurrent(concurrency: Int) extends AdventConcurrency

case class AdventContext(
  start: Instant = Time.now,
  end: Option[Instant] = None,
  concurrency: AdventConcurrency = AdventSerial,
  results: List[DayResult] = Nil
) {
  def elapsed: Duration = Time.diff(start, end.getOrElse(Time.now))
}
