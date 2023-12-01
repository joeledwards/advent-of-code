package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.util.FileSystem

class RegexExtractionTests extends UnitSpec {
  "a regex" when {
    "used for pattern matching extraction" should {
      "extract from example" in {
        val NumericValue = "^(?:.*[^1-9])?([0-9]+)(?:[^0-9].*)?$".r

        assert(("day01.txt" match {
          case NumericValue(d) if d.toInt == 1 => Some(d.toInt)
          case _ => None
        }).contains(1))
      }

      "extract from actual file list" in {
        val DayFileName = "^(?:.*[^1-9])?([0-9]+)(?:[^0-9].*)?$".r
        val fileName = List("day1.txt", "day12.txt", "day2.txt", "day02.txt")
          .map(d => (d, d))
          .collectFirst {
            case (f, DayFileName(d)) if d.toInt == 2 => f
          }
        assert(fileName.contains("day2.txt"))
      }

      "match the expected patterns" in {
        val DayFileName = "^(?:.*[^1-9])?([0-9]+)(?:[^0-9].*)?$".r
        assert(DayFileName.matches("day0.txt"))
        assert(DayFileName.matches("day0.txt"))
        assert(DayFileName.matches("day01.txt"))
        assert(DayFileName.matches("day02.txt"))
        assert(DayFileName.matches("day10.txt"))
        assert(DayFileName.matches("day010.txt"))
      }
    }
  }
}
