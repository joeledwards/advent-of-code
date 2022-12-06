package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day4._

import scala.collection.immutable.SortedSet

class day4Spec extends UnitSpec {
  "day4.parseLine" when {
    "parsing an input line" should {
      "parse valid input" in {
        parseLine("1-1,2-2") shouldBe Some((SortedSet(1), SortedSet(2)))
        parseLine("1-2,3-4") shouldBe Some((SortedSet(1, 2), SortedSet(3, 4)))
        parseLine("1-3,2-4") shouldBe Some((SortedSet(1, 2, 3), SortedSet(2, 3, 4)))
      }
      
      "reject invalid input" in {
        parseLine("") shouldBe None
        parseLine("1-2") shouldBe None
        parseLine(",1-2") shouldBe None
        parseLine("1-2 1-2") shouldBe None
        parseLine("1:2,1:2") shouldBe None
      }
    }
  }
}
