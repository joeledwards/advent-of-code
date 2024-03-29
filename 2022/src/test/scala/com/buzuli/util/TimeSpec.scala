package com.buzuli.util

import scala.concurrent.duration._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import java.util.concurrent.TimeUnit

class TimeSpec extends AnyWordSpec with Matchers {
  "diff" when {
    "comparing instants" should {
      "report the correct difference" in {
        def m(milli: Long): Instant = Instant.ofEpochMilli(milli)
        def d(millis: Long): Duration = Duration(millis, TimeUnit.MILLISECONDS)

        Time.diff(m(1), m(1)) should equal(d(0))
        Time.diff(m(0), m(1)) should equal(d(1))
        Time.diff(m(0), m(10)) should equal(d(10))
        Time.diff(m(1), m(0)) should equal(d(-1))
      }
    }
  }

  "since" when {
    "comparing to an instant" should {
      "report the correct difference" in {
        val now = Time.now
        assert(Time.since(now).toMillis >= 0)

        assert(Time.since(now, now).toMillis == 0)
        assert(Time.since(now, now.plusMillis(1)).toMillis == 1)
        assert(Time.since(now, now.minusMillis(1)).toMillis == -1)
      }
    }
  }

  "prettyDuration" when {
    "formatting durations" should {
      "correctly format sub-millisecond durations" in {
        assert(Time.prettyDuration(1.nanosecond) == "0.001us")
        assert(Time.prettyDuration(10.nanoseconds) == "0.010us")
        assert(Time.prettyDuration(100.nanoseconds) == "0.100us")
        assert(Time.prettyDuration(1000.nanoseconds) == "1.000us")
        assert(Time.prettyDuration(1.microsecond) == "1.000us")
        assert(Time.prettyDuration(1001.nanoseconds) == "1.001us")
        assert(Time.prettyDuration(1010.nanoseconds) == "1.010us")
        assert(Time.prettyDuration(1999.nanoseconds) == "1.999us")
        assert(Time.prettyDuration(999.microseconds) == "999.000us")
        assert(Time.prettyDuration(999999.nanoseconds) == "999.999us")
      }

      "correctly format sub-second durations" in {
        assert(Time.prettyDuration(1.millisecond) == "1.000ms")
        assert(Time.prettyDuration(1000.microseconds) == "1.000ms")
        assert(Time.prettyDuration(10.milliseconds) == "10.000ms")
        assert(Time.prettyDuration(10000.microseconds) == "10.000ms")
        assert(Time.prettyDuration(100.milliseconds) == "100.000ms")
        assert(Time.prettyDuration(1000.microseconds) == "1.000ms")
        assert(Time.prettyDuration(1001.microseconds) == "1.001ms")
        assert(Time.prettyDuration(1010.microseconds) == "1.010ms")
        assert(Time.prettyDuration(1999.microseconds) == "1.999ms")
        assert(Time.prettyDuration(999999.microseconds) == "999.999ms")
      }

      "correctly format sub-minute durations" in {
        assert(Time.prettyDuration(1.second) == "1.000s")
        assert(Time.prettyDuration(1000.milliseconds) == "1.000s")
        assert(Time.prettyDuration(1001.milliseconds) == "1.001s")
        assert(Time.prettyDuration(1999.milliseconds) == "1.999s")
        assert(Time.prettyDuration(2.seconds) == "2.000s")
        assert(Time.prettyDuration(2000.milliseconds) == "2.000s")
        assert(Time.prettyDuration(2001.milliseconds) == "2.001s")
        assert(Time.prettyDuration(59.seconds) == "59.000s")
        assert(Time.prettyDuration(59999.milliseconds) == "59.999s")
        assert(Time.prettyDuration(59999999.microseconds) == "59.999s")
      }

      "correctly format sub-hour durations" in {
        assert(Time.prettyDuration(1.minute) == "1m 0s")
        assert(Time.prettyDuration(60.seconds) == "1m 0s")
        assert(Time.prettyDuration(60000.milliseconds) == "1m 0s")
        assert(Time.prettyDuration(62000.milliseconds) == "1m 2s")
        assert(Time.prettyDuration(61.seconds) == "1m 1s")
        assert(Time.prettyDuration(71.seconds) == "1m 11s")
        assert(Time.prettyDuration(119.seconds) == "1m 59s")
        assert(Time.prettyDuration(59.minutes) == "59m 0s")
        assert(Time.prettyDuration(3599.seconds) == "59m 59s")
        assert(Time.prettyDuration(3599999.milliseconds) == "59m 59s")
      }

      "correctly format sub-day durations" in {
        assert(Time.prettyDuration(1.hour) == "1h 0m")
        assert(Time.prettyDuration(60.minutes) == "1h 0m")
        assert(Time.prettyDuration(61.minutes) == "1h 1m")
        assert(Time.prettyDuration(71.minutes) == "1h 11m")
        assert(Time.prettyDuration(119.minutes) == "1h 59m")
        assert(Time.prettyDuration(2.hour) == "2h 0m")
        assert(Time.prettyDuration(1439.minutes) == "23h 59m")
      }

      "correctly format day durations" in {
        assert(Time.prettyDuration(1.day) == "1d 0h")
        assert(Time.prettyDuration(24.hours) == "1d 0h")
        assert(Time.prettyDuration(1440.minutes) == "1d 0h")
        assert(Time.prettyDuration(25.hours) == "1d 1h")
        assert(Time.prettyDuration(35.hours) == "1d 11h")
        assert(Time.prettyDuration(47.hours) == "1d 23h")
        assert(Time.prettyDuration(2.day) == "2d 0h")
      }
    }
  }
}