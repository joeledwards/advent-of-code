package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day5.isValidUpdate

class day5spec extends UnitSpec {
  "isValidUpdate" when {
    "receiving no rules" should {
      "always return true" in {
        isValidUpdate(List(), List(3, 2, 1)) shouldBe true
      }
    }

    "receiving an invalid update" should {
      "return false" in {
        isValidUpdate(List(2 -> 1), List(1, 2, 3)) shouldBe false
      }
    }

    "receiving an valid update" should {
      "return false" in {
        isValidUpdate(List(2 -> 1), List(2, 1, 3)) shouldBe true
      }
    }

    "sparse rules" should {
      "return false" in {
        isValidUpdate(List(4 -> 1), List(2, 1, 5, 4, 3)) shouldBe false
      }
    }
  }
}