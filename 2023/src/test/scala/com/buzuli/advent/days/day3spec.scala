package com.buzuli.advent.days

import com.buzuli.UnitSpec

import com.buzuli.advent.days.day3._

class day3spec extends UnitSpec {
  "day3.Symbol" when {
    "providing adjacent points" should {
      "contain all 8 adjacencies" in {
        Symbol('A', Point(1, 1)).adjacentPoints shouldBe Set(
          Point(0, 0),
          Point(0, 1),
          Point(0, 2),
          Point(1, 0),
          Point(1, 2),
          Point(2, 0),
          Point(2, 1),
          Point(2, 2),
        )
      }
    }
  }

  "day3.NumericWord" when {
    "providing contained points" should {
      "contain all internal points" in {
        NumericWord("123", Point(1, 1)).containedPoints shouldBe Set(
          Point(1, 1),
          Point(2, 1),
          Point(3, 1),
        )
      }
    }

    "providing adjacent points" should {
      "contain adjacencies for all surrounding, non-internal points" in {
        NumericWord("123", Point(1, 1)).adjacentPoints shouldBe Set(
          Point(0, 0),
          Point(0, 1),
          Point(0, 2),
          Point(1, 0),
          Point(1, 2),
          Point(2, 0),
          Point(2, 2),
          Point(3, 0),
          Point(3, 2),
          Point(4, 0),
          Point(4, 1),
          Point(4, 2),
        )
      }
    }
  }

  "day3.parseLine" when {
    "parsing an empty line" should {
      "return no MapElements" in {
        parseLine("", 0) shouldBe Nil
      }
    }

    "parsing a line with symbols" should {
      "return symbols" in {
        parseLine("@.##.$", 0) shouldBe List(
          Symbol('@', Point(0, 0)),
          Symbol('#', Point(2, 0)),
          Symbol('#', Point(3, 0)),
          Symbol('$', Point(5, 0)),
        )
      }
    }

    "parsing a line with numbers" should {
      "return numbers" in {
        parseLine("1.23.456.78.9", 0) shouldBe List(
          NumericWord("1", Point(0, 0)),
          NumericWord("23", Point(2, 0)),
          NumericWord("456", Point(5, 0)),
          NumericWord("78", Point(9, 0)),
          NumericWord("9", Point(12, 0)),
        )
      }
    }

    "parsing a line with numbers and symbols" should {
      "return both numbers and symbols" in {
        parseLine("1.@.2$3.q.4", 0) shouldBe List(
          NumericWord("1", Point(0, 0)),
          Symbol('@', Point(2, 0)),
          NumericWord("2", Point(4, 0)),
          Symbol('$', Point(5, 0)),
          NumericWord("3", Point(6, 0)),
          Symbol('q', Point(8, 0)),
          NumericWord("4", Point(10, 0)),
        )
      }
    }
  }
}