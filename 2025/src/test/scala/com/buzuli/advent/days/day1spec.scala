package com.buzuli.advent.days

import com.buzuli.UnitSpec

class day1spec extends UnitSpec {
  "Dial.rotate()" when {
    "Applying a displacement" should {
      "Correctly compute the new offset" in {
        Dial(0, 0).rotate(Displacement(0)).offset shouldBe 0
        Dial(0, 0).rotate(Displacement(1)).offset shouldBe 1
        Dial(0, 0).rotate(Displacement(-1)).offset shouldBe 99
        Dial(0, 0).rotate(Displacement(10)).offset shouldBe 10
        Dial(0, 0).rotate(Displacement(-10)).offset shouldBe 90
        Dial(0, 0).rotate(Displacement(99)).offset shouldBe 99
        Dial(0, 0).rotate(Displacement(-99)).offset shouldBe 1

        Dial(50, 0).rotate(Displacement(50)).offset shouldBe 0
        Dial(50, 0).rotate(Displacement(-50)).offset shouldBe 0

        Dial(0, 0).rotate(Displacement(100)).offset shouldBe 0
        Dial(0, 0).rotate(Displacement(-100)).offset shouldBe 0
        Dial(0, 0).rotate(Displacement(101)).offset shouldBe 1
        Dial(0, 0).rotate(Displacement(-101)).offset shouldBe 99
        Dial(0, 0).rotate(Displacement(110)).offset shouldBe 10
        Dial(0, 0).rotate(Displacement(-110)).offset shouldBe 90
        Dial(0, 0).rotate(Displacement(111)).offset shouldBe 11
        Dial(0, 0).rotate(Displacement(-111)).offset shouldBe 89
      }

      "Correctly calculate the zeros" in {
        Dial(50, 0).rotate(Displacement(0)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(1)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-1)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(10)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-10)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(99)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-99)).zeros shouldBe 0

        Dial(50, 0).rotate(Displacement(100)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-100)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(101)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-101)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(110)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-110)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(111)).zeros shouldBe 0
        Dial(50, 0).rotate(Displacement(-111)).zeros shouldBe 0

        Dial(50, 0).rotate(Displacement(50)).zeros shouldBe 1
        Dial(50, 0).rotate(Displacement(-50)).zeros shouldBe 1
        Dial(50, 0).rotate(25).rotate(25).zeros shouldBe 1
        Dial(50, 0).rotate(-25).rotate(-25).zeros shouldBe 1

        Dial(50, 0).rotate(50).rotate(-100).zeros shouldBe 2
        Dial(50, 0).rotate(-50).rotate(100).zeros shouldBe 2

        Dial(50, 0).rotate(50).rotate(-50).rotate(50).zeros shouldBe 2
        Dial(50, 0).rotate(-50).rotate(50).rotate(50).zeros shouldBe 2
        Dial(50, 0).rotate(50).rotate(-50).rotate(-50).zeros shouldBe 2
      }
    }
  }
}