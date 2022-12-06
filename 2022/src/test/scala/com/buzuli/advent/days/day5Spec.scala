package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day5._

class day5Spec extends UnitSpec {
  "day5.parseStacks" when {
    "parsing stacks with IDs" should {
      "correctly align stacks" in {
        val input = List(
          "    [D]",
          "[N] [C]",
          "[Z] [M] [P]",
          " 1   2   3",
        )
        
        parseStacks(input) shouldBe Map(
          1 -> List('N', 'Z'),
          2 -> List('D', 'C', 'M'),
          3 -> List('P'),
        )
      }
    }
  }
  
  "day5.parseMove" when {
    "parsing a move instruction line" should {
      "parse a valid move instruction" in {
        parseMove("move 5 from 3 to 1") shouldBe Some(Move(5, 3, 1))
        parseMove("move 20 from 03 to 99") shouldBe Some(Move(20, 3, 99))
      }
      
      "reject an invalid move instruction" in {
        parseMove(" move 1 from 2 to 3") shouldBe None
        parseMove("move1 from 2 to 3") shouldBe None
        parseMove("move 1from 2 to 3") shouldBe None
        parseMove("move 1 from2 to 3") shouldBe None
        parseMove("move 1 from 2to 3") shouldBe None
        parseMove("move 1 from 2 to3") shouldBe None
        parseMove("move 1 from 2 to 3 ") shouldBe None
      }
    }
  }
}
