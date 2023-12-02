package com.buzuli.advent.days

import com.buzuli.UnitSpec

class day1spec extends UnitSpec {
  "day1.firstDigitWord" when {
    "parsing an empty string" should {
      "return None" in {
        day1.firstDigitOrWord("") shouldBe None
      }
    }

    "parsing a string with no digit values" should {
      "return None" in {
        day1.firstDigitOrWord("ontow") shouldBe None
      }
    }

    "parsing a string with a single digit character" should {
      "return the value and its index" in {
        day1.firstDigitOrWord("3ab") shouldBe Some(3)
        day1.firstDigitOrWord("a3b") shouldBe Some(3)
        day1.firstDigitOrWord("ab3") shouldBe Some(3)
      }
    }

    "parsing a string with a single digit word" should {
      "return the value and its index" in {
        day1.firstDigitOrWord("threeab") shouldBe Some(3)
        day1.firstDigitOrWord("athreeb") shouldBe Some(3)
        day1.firstDigitOrWord("abthree") shouldBe Some(3)
      }
    }

    "parsing a string with multiple digit values" should {
      "return the first value and its index" in {
        day1.firstDigitOrWord("1two") shouldBe Some(1)
        day1.firstDigitOrWord("one2") shouldBe Some(1)
        day1.firstDigitOrWord("ontwothree") shouldBe Some(2)
        day1.firstDigitOrWord("oneight") shouldBe Some(1)
      }
    }

    "parsing digit words" should {
      "return the value for all valid words" in {
        day1.firstDigitOrWord("zero") shouldBe Some(0)
        day1.firstDigitOrWord("one") shouldBe Some(1)
        day1.firstDigitOrWord("two") shouldBe Some(2)
        day1.firstDigitOrWord("three") shouldBe Some(3)
        day1.firstDigitOrWord("four") shouldBe Some(4)
        day1.firstDigitOrWord("five") shouldBe Some(5)
        day1.firstDigitOrWord("six") shouldBe Some(6)
        day1.firstDigitOrWord("seven") shouldBe Some(7)
        day1.firstDigitOrWord("eight") shouldBe Some(8)
        day1.firstDigitOrWord("nine") shouldBe Some(9)
      }
    }
  }

  "day1.lastDigitWord" when {
    "parsing an empty string" should {
      "return None" in {
        day1.lastDigitOrWord("") shouldBe None
      }
    }

    "parsing a string with no digit values" should {
      "return None" in {
        day1.lastDigitOrWord("ontow") shouldBe None
      }
    }

    "parsing a string with a single digit character" should {
      "return the value and its index" in {
        day1.lastDigitOrWord("3ab") shouldBe Some(3)
        day1.lastDigitOrWord("a3b") shouldBe Some(3)
        day1.lastDigitOrWord("ab3") shouldBe Some(3)
      }
    }

    "parsing a string with a single digit word" should {
      "return the value and its index" in {
        day1.lastDigitOrWord("threeab") shouldBe Some(3)
        day1.lastDigitOrWord("athreeb") shouldBe Some(3)
        day1.lastDigitOrWord("abthree") shouldBe Some(3)
      }
    }

    "parsing a string with multiple digit values" should {
      "return the last value and its index" in {
        day1.lastDigitOrWord("1two") shouldBe Some(2)
        day1.lastDigitOrWord("one2") shouldBe Some(2)
        day1.lastDigitOrWord("ontwothree") shouldBe Some(3)
        day1.lastDigitOrWord("oneight") shouldBe Some(8)
        day1.lastDigitOrWord("two1nine") shouldBe Some(9)
      }
    }
  }

  "parsing digit words" should {
    "return the value for all valid words" in {
      day1.lastDigitOrWord("zero") shouldBe Some(0)
      day1.lastDigitOrWord("one") shouldBe Some(1)
      day1.lastDigitOrWord("two") shouldBe Some(2)
      day1.lastDigitOrWord("three") shouldBe Some(3)
      day1.lastDigitOrWord("four") shouldBe Some(4)
      day1.lastDigitOrWord("five") shouldBe Some(5)
      day1.lastDigitOrWord("six") shouldBe Some(6)
      day1.lastDigitOrWord("seven") shouldBe Some(7)
      day1.lastDigitOrWord("eight") shouldBe Some(8)
      day1.lastDigitOrWord("nine") shouldBe Some(9)
    }
  }
}