package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day8._

class day8Spec extends UnitSpec {
  "day8.transposeGrid" when {
    "rotating nested lists" should {
      "generate the expected rotation" in {
        transposeGrid(List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9),
        )) shouldBe List(
          List(1, 4, 7),
          List(2, 5, 8),
          List(3, 6, 9),
        )
        
        transposeGrid(List(
          List(1, 2),
          List(3, 4),
          List(5, 6),
        )) shouldBe List(
          List(1, 3, 5),
          List(2, 4, 6),
        )
        
        transposeGrid(List(
          List(1, 2, 3),
          List(4, 5, 6),
        )) shouldBe List(
          List(1, 4),
          List(2, 5),
          List(3, 6)
        )
      }
    }
  }
}
