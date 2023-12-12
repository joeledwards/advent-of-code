package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day6._

class day6spec extends UnitSpec {
  "day6.findFirstAbove" when {
    "inspecting a Race" should {
      "return the earliest lower value" in {
        findFirstAbove(Race(10, 16), 0, 5) shouldBe 3L
      }
    }
  }
}