package com.buzuli.advent.days

import com.buzuli.UnitSpec

class day1spec extends UnitSpec {
  "Dial.rotate()" when {
    "Applying a displacement" should {
      "Correctly compute the new offset" in {
        Dial(0, 0, 0).rotate(Displacement(0)).offset shouldBe 0
        Dial(0, 0, 0).rotate(Displacement(1)).offset shouldBe 1
        Dial(0, 0, 0).rotate(Displacement(-1)).offset shouldBe 99
        Dial(0, 0, 0).rotate(Displacement(10)).offset shouldBe 10
        Dial(0, 0, 0).rotate(Displacement(-10)).offset shouldBe 90
        Dial(0, 0, 0).rotate(Displacement(99)).offset shouldBe 99
        Dial(0, 0, 0).rotate(Displacement(-99)).offset shouldBe 1

        Dial(50, 0, 0).rotate(Displacement(50)).offset shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-50)).offset shouldBe 0

        Dial(0, 0, 0).rotate(Displacement(100)).offset shouldBe 0
        Dial(0, 0, 0).rotate(Displacement(-100)).offset shouldBe 0
        Dial(0, 0, 0).rotate(Displacement(101)).offset shouldBe 1
        Dial(0, 0, 0).rotate(Displacement(-101)).offset shouldBe 99
        Dial(0, 0, 0).rotate(Displacement(110)).offset shouldBe 10
        Dial(0, 0, 0).rotate(Displacement(-110)).offset shouldBe 90
        Dial(0, 0, 0).rotate(Displacement(111)).offset shouldBe 11
        Dial(0, 0, 0).rotate(Displacement(-111)).offset shouldBe 89
      }

      "Correctly calculate the stop zeros" in {
        Dial(50, 0, 0).rotate(Displacement(0)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(1)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-1)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(10)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-10)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(99)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-99)).stopZeros shouldBe 0

        Dial(50, 0, 0).rotate(Displacement(100)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-100)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(101)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-101)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(110)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-110)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(111)).stopZeros shouldBe 0
        Dial(50, 0, 0).rotate(Displacement(-111)).stopZeros shouldBe 0

        Dial(50, 0, 0).rotate(Displacement(50)).stopZeros shouldBe 1
        Dial(50, 0, 0).rotate(Displacement(-50)).stopZeros shouldBe 1
        Dial(50, 0, 0).rotate(25).rotate(25).stopZeros shouldBe 1
        Dial(50, 0, 0).rotate(-25).rotate(-25).stopZeros shouldBe 1

        Dial(50, 0, 0).rotate(50).rotate(-100).stopZeros shouldBe 2
        Dial(50, 0, 0).rotate(-50).rotate(100).stopZeros shouldBe 2

        Dial(50, 0, 0).rotate(50).rotate(-50).rotate(50).stopZeros shouldBe 2
        Dial(50, 0, 0).rotate(-50).rotate(50).rotate(50).stopZeros shouldBe 2
        Dial(50, 0, 0).rotate(50).rotate(-50).rotate(-50).stopZeros shouldBe 2
      }
    }

    "Correctly calculate the passed zeros" in {
      Dial(50, 0, 0).rotate(Displacement(0)).clickZeros shouldBe 0
      Dial(50, 0, 0).rotate(Displacement(1)).clickZeros shouldBe 0
      Dial(50, 0, 0).rotate(Displacement(-1)).clickZeros shouldBe 0
      Dial(50, 0, 0).rotate(Displacement(10)).clickZeros shouldBe 0
      Dial(50, 0, 0).rotate(Displacement(-10)).clickZeros shouldBe 0

      Dial(50, 0, 0).rotate(Displacement(99)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(-99)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(100)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(-100)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(101)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(-101)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(110)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(-110)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(111)).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(Displacement(-111)).clickZeros shouldBe 1

      Dial(50, 0, 0).rotate(50).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(-50).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(25).rotate(25).clickZeros shouldBe 1
      Dial(50, 0, 0).rotate(-25).rotate(-25).clickZeros shouldBe 1

      Dial(50, 0, 0).rotate(50).rotate(100).clickZeros shouldBe 2
      Dial(50, 0, 0).rotate(50).rotate(-100).clickZeros shouldBe 2
      Dial(50, 0, 0).rotate(-50).rotate(100).clickZeros shouldBe 2
      Dial(50, 0, 0).rotate(-50).rotate(-100).clickZeros shouldBe 2

      Dial(50, 0, 0).rotate(50).rotate(-50).rotate(50).clickZeros shouldBe 2
      Dial(50, 0, 0).rotate(-50).rotate(50).rotate(50).clickZeros shouldBe 2
      Dial(50, 0, 0).rotate(50).rotate(-50).rotate(-50).clickZeros shouldBe 2

      Dial(50, 0, 0).rotate(100).rotate(100).rotate(100).clickZeros shouldBe 3
      Dial(50, 0, 0).rotate(100).rotate(100).rotate(50).clickZeros shouldBe 3

      Dial(50, 0, 0).rotate(250).clickZeros shouldBe 3
      Dial(50, 0, 0).rotate(300).clickZeros shouldBe 3
      Dial(50, 0, 0).rotate(500).clickZeros shouldBe 5
      Dial(50, 0, 0).rotate(549).clickZeros shouldBe 5
      Dial(50, 0, 0).rotate(550).clickZeros shouldBe 6
      Dial(50, 0, 0).rotate(551).clickZeros shouldBe 6
    }

    "DEBUG" in {
      Dial(50, 0, 0).rotate(50).rotate(-50).rotate(50).clickZeros shouldBe 2
    }
  }
}