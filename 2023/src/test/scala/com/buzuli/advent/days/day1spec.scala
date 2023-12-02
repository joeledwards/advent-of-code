package com.buzuli.advent.days

import com.buzuli.UnitSpec

class day1spec extends UnitSpec {
  "day1.firstDigitWord" when {
    "parsing an empty string" should {
      "return None" in {
        day1.firstDigitWord("") shouldBe None
      }
    }

    "parsing a string with no digit values" should {
      "return None" in {
        day1.firstDigitWord("ontow") shouldBe None
      }
    }

    "parsing a string with a single digit character" should {
      "return the value and its index" in {
        day1.firstDigitWord("3ab") shouldBe Some((3, 0))
        day1.firstDigitWord("a3b") shouldBe Some((3, 1))
        day1.firstDigitWord("ab3") shouldBe Some((3, 2))
      }
    }

    "parsing a string with a single digit word" should {
      "return the value and its index" in {
        day1.firstDigitWord("threeab") shouldBe Some((3, 0))
        day1.firstDigitWord("athreeb") shouldBe Some((3, 1))
        day1.firstDigitWord("abthree") shouldBe Some((3, 2))
      }
    }

    "parsing a string with multiple digit values" should {
      "return the first value and its index" in {
        day1.firstDigitWord("1two") shouldBe Some((1, 0))
        day1.firstDigitWord("one2") shouldBe Some((1, 0))
        day1.firstDigitWord("ontwothree") shouldBe Some((2, 2))
        day1.firstDigitWord("oneight") shouldBe Some((1, 0))
      }
    }

    "parsing digit words" should {
      "return the value for all valid words" in {
        day1.firstDigitWord("zero") shouldBe Some((0, 0))
        day1.firstDigitWord("one") shouldBe Some((1, 0))
        day1.firstDigitWord("two") shouldBe Some((2, 0))
        day1.firstDigitWord("three") shouldBe Some((3, 0))
        day1.firstDigitWord("four") shouldBe Some((4, 0))
        day1.firstDigitWord("five") shouldBe Some((5, 0))
        day1.firstDigitWord("six") shouldBe Some((6, 0))
        day1.firstDigitWord("seven") shouldBe Some((7, 0))
        day1.firstDigitWord("eight") shouldBe Some((8, 0))
        day1.firstDigitWord("nine") shouldBe Some((9, 0))
      }
    }
  }

  "day1.lastDigitWord" when {
    "parsing an empty string" should {
      "return None" in {
        day1.lastDigitWord("") shouldBe None
      }
    }

    "parsing a string with no digit values" should {
      "return None" in {
        day1.lastDigitWord("ontow") shouldBe None
      }
    }

    "parsing a string with a single digit character" should {
      "return the value and its index" in {
        day1.lastDigitWord("3ab") shouldBe Some((3, 0))
        day1.lastDigitWord("a3b") shouldBe Some((3, 1))
        day1.lastDigitWord("ab3") shouldBe Some((3, 2))
      }
    }

    "parsing a string with a single digit word" should {
      "return the value and its index" in {
        day1.lastDigitWord("threeab") shouldBe Some((3, 0))
        day1.lastDigitWord("athreeb") shouldBe Some((3, 1))
        day1.lastDigitWord("abthree") shouldBe Some((3, 2))
      }
    }

    "parsing a string with multiple digit values" should {
      "return the last value and its index" in {
        day1.lastDigitWord("1two") shouldBe Some((2, 1))
        day1.lastDigitWord("one2") shouldBe Some((2, 3))
        day1.lastDigitWord("ontwothree") shouldBe Some((3, 5))
        day1.lastDigitWord("oneight") shouldBe Some((8, 2))
        day1.lastDigitWord("two1nine") shouldBe Some((9, 4))
      }
    }
  }

  "parsing digit words" should {
    "return the value for all valid words" in {
      day1.lastDigitWord("zero") shouldBe Some((0, 0))
      day1.lastDigitWord("one") shouldBe Some((1, 0))
      day1.lastDigitWord("two") shouldBe Some((2, 0))
      day1.lastDigitWord("three") shouldBe Some((3, 0))
      day1.lastDigitWord("four") shouldBe Some((4, 0))
      day1.lastDigitWord("five") shouldBe Some((5, 0))
      day1.lastDigitWord("six") shouldBe Some((6, 0))
      day1.lastDigitWord("seven") shouldBe Some((7, 0))
      day1.lastDigitWord("eight") shouldBe Some((8, 0))
      day1.lastDigitWord("nine") shouldBe Some((9, 0))
    }
  }
}
