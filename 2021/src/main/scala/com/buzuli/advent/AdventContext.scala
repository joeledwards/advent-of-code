package com.buzuli.advent

import com.buzuli.util.Time

import java.time.Instant
import scala.concurrent.duration.Duration

sealed trait AdventConcurrency
case object AdventSerial extends AdventConcurrency
case class AdventConcurrent(concurrency: Int) extends AdventConcurrency {
  assert(concurrency > 0 && concurrency < 26)
}

case class AdventContext(
  start: Instant = Time.now,
  end: Option[Instant] = None,
  concurrency: AdventConcurrency = AdventSerial,
  results: List[DayResult] = Nil
) {
  def elapsed: Duration = Time.diff(start, end.getOrElse(Time.now))
}
