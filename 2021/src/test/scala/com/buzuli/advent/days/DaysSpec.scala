package com.buzuli.advent.days

import com.buzuli.UnitSpec

import scala.language.postfixOps

class DaysSpec extends UnitSpec {
  "Days" when {
    "listing days" should {
      "have unique names for all days" in {
        assert(Days.dayList.map(_.name).toSet.size == 25)
      }

      "have the expected number of each day" in {
        for ((dayNumber, day) <- (1 until 25 toList).zip(Days.dayList)) {
          day.day should equal(dayNumber)
          day.name should equal(s"Day ${dayNumber}")
        }
      }
    }
  }
}
