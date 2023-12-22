package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day11._

class day11spec extends UnitSpec {
  "day11.hash" when {
    "expanding a universe" should {
      "return updated Galaxy objects" in {
        expanded(List(Galaxy(0,0,0))) shouldBe List(Galaxy(0,0,0))

        expanded(List(Galaxy(0,1,0))) shouldBe List(Galaxy(0,2,0))
        expanded(List(Galaxy(0,0,1))) shouldBe List(Galaxy(0,0,2))

        expanded(List(Galaxy(0, 2, 0))) shouldBe List(Galaxy(0, 4, 0))
        expanded(List(Galaxy(0, 0, 2))) shouldBe List(Galaxy(0, 0, 4))

        expanded(List(Galaxy(0,1,1))) shouldBe List(Galaxy(0,2,2))
        expanded(List(Galaxy(0,2,3))) shouldBe List(Galaxy(0,4,6))
        expanded(List(Galaxy(0,3,2))) shouldBe List(Galaxy(0,6,4))

        expanded(List(Galaxy(1,1,1), Galaxy(2,2,2))) shouldBe List(Galaxy(1,2,2), Galaxy(2,3,3))
        expanded(List(Galaxy(1,1,1), Galaxy(2,3,3))) shouldBe List(Galaxy(1,2,2), Galaxy(2,5,5))
        expanded(List(Galaxy(1,1,2), Galaxy(2,3,4))) shouldBe List(Galaxy(1,2,4), Galaxy(2,5,7))

        expanded(List(Galaxy(1, 1, 1), Galaxy(2, 2, 2)), 2) shouldBe List(Galaxy(1, 3, 3), Galaxy(2, 4, 4))
        expanded(List(Galaxy(1, 1, 1), Galaxy(2, 3, 3)), 2) shouldBe List(Galaxy(1, 3, 3), Galaxy(2, 7, 7))
        expanded(List(Galaxy(1, 1, 2), Galaxy(2, 3, 4)), 2) shouldBe List(Galaxy(1, 3, 6), Galaxy(2, 7, 10))
      }
    }
  }
}