package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day5._

class day5spec extends UnitSpec {
  "day5.RangeMap.find" when {
    "analyzing a contained value" should {
      "return the mapped value" in {
        RangeMap(1, 1, 1).find(1) shouldBe Some(1)

        RangeMap(2, 2, 2).find(2) shouldBe Some(2)
        RangeMap(2, 2, 2).find(3) shouldBe Some(3)

        RangeMap(1, 2, 1).find(1) shouldBe Some(2)

        RangeMap(50, 98, 2).find(50) shouldBe Some(98)
        RangeMap(50, 98, 2).find(51) shouldBe Some(99)
      }
    }

    "analyzing a missing value" should {
      "return None" in {
        RangeMap(1, 1, 1).find(2) shouldBe None

        RangeMap(2, 2, 2).find(1) shouldBe None
        RangeMap(2, 2, 2).find(4) shouldBe None

        RangeMap(1, 2, 1).find(3) shouldBe None

        RangeMap(50, 98, 2).find(49) shouldBe None
        RangeMap(50, 98, 2).find(52) shouldBe None
      }
    }
  }

  "day5.Mapper" when {
    "mapping a contained value" should {
      "return the mapped value" in {
        RangeMapper("a", "b", List(RangeMap(1, 1, 1))).map(1) shouldBe 1
        RangeMapper("a", "b", List(RangeMap(1, 1, 2))).map(2) shouldBe 2
        RangeMapper("a", "b", List(RangeMap(1, 2, 2))).map(2) shouldBe 3
        RangeMapper("a", "b", List(RangeMap(50, 98, 2))).map(50) shouldBe 98
        RangeMapper("a", "b", List(RangeMap(50, 98, 2))).map(51) shouldBe 99

        RangeMapper("a", "b", List(RangeMap(1, 1, 1), RangeMap(2, 2, 2))).map(2) shouldBe 2
        RangeMapper("a", "b", List(RangeMap(1, 2, 1), RangeMap(2, 3, 2))).map(2) shouldBe 3

        RangeMapper("a", "b", List(RangeMap(1, 2, 2), RangeMap(2, 4, 2))).map(2) shouldBe 3
        RangeMapper("a", "b", List(RangeMap(1, 3, 2), RangeMap(2, 1, 2))).map(2) shouldBe 4
      }
    }

    "mapping a non-contained value" should {
      "return the original value" in {
        RangeMapper("a", "b", List(RangeMap(1, 1, 1))).map(2) shouldBe 2
        RangeMapper("a", "b", List(RangeMap(1, 1, 2))).map(3) shouldBe 3
        RangeMapper("a", "b", List(RangeMap(1, 2, 2))).map(3) shouldBe 3

        RangeMapper("a", "b", List(RangeMap(50, 98, 2))).map(49) shouldBe 49
        RangeMapper("a", "b", List(RangeMap(50, 98, 2))).map(52) shouldBe 52
      }
    }
  }
}