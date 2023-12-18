package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day9.{extrapolateNextValue, _}

class day9spec extends UnitSpec {
  def parse(str: String): List[Long] = str.split(" ").flatMap(_.toLongOption).toList

  "day9.extrapolateNextValue" when {
    "inspecting a value sequence" should {
      "return the next value in the sequence" in {
        extrapolateNextValue(List(0, 3, 6).map(_.toLong)) shouldBe 9
        extrapolateNextValue(List(0, 3, 6, 9).map(_.toLong)) shouldBe 12
        extrapolateNextValue(List(0, 3, 6, 9, 12).map(_.toLong)) shouldBe 15
        extrapolateNextValue(List(0, 3, 6, 9, 12, 15).map(_.toLong)) shouldBe 18

        extrapolateNextValue(List(1, 3, 6).map(_.toLong)) shouldBe 10
        extrapolateNextValue(List(1, 3, 6, 10).map(_.toLong)) shouldBe 15
        extrapolateNextValue(List(1, 3, 6, 10, 15).map(_.toLong)) shouldBe 21
        extrapolateNextValue(List(1, 3, 6, 10, 15, 21).map(_.toLong)) shouldBe 28


        // 10  13  16  19
        //    3   3   3
        //      0   0
        extrapolateNextValue(List(10, 13, 16).map(_.toLong)) shouldBe 19

        // 10  13  16  21  30
        //    3   3   5   9
        //      0   2   4
        //        2   2
        //        0 0
        extrapolateNextValue(List(10, 13, 16, 21).map(_.toLong)) shouldBe 30
        extrapolateNextValue(List(10, 13, 16, 21, 30).map(_.toLong)) shouldBe 45
        extrapolateNextValue(List(10, 13, 16, 21, 30, 45).map(_.toLong)) shouldBe 68

        extrapolateNextValue(List(1, 2, 3).map(_.toLong)) shouldBe 4
        extrapolateNextValue(List(-1, -2, -3).map(_.toLong)) shouldBe -4

        extrapolateNextValue(List(2, 4).map(_.toLong)) shouldBe 6
        extrapolateNextValue(List(2, 4, 8).map(_.toLong)) shouldBe 14
        extrapolateNextValue(List(-5, 4, 13).map(_.toLong)) shouldBe 22

        extrapolateNextValue(
          parse("15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75")
        ) shouldBe 78

        extrapolateNextValue(
          parse("-3 -1 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37")
        ) shouldBe 39

        extrapolateNextValue(
          parse("22 30 34 34 30 22 10 -6 -26 -50 -78 -110 -146 -186 -230 -278 -330 -386 -446 -510 -578")
        ) shouldBe -650

        extrapolateNextValue(
          parse("2 -3 -8 -13 -18 -23 -28 -33 -38 -43 -48 -53 -58 -63 -68 -73 -78 -83 -88 -93 -98")
        ) shouldBe -103
      }
    }
  }
}