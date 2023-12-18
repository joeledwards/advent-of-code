package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day15._

class day15spec extends UnitSpec {
  "day15.hash" when {
    "hashing a string" should {
      "return the correct hash value" in {
        hash("rn=1") shouldBe 30
        hash("cm-") shouldBe 253
        hash("qp=3") shouldBe 97
        hash("cm=2") shouldBe 47
        hash("qp-") shouldBe 14
        hash("pc=4") shouldBe 180
        hash("ot=9") shouldBe 9
        hash("ab=5") shouldBe 197
        hash("pc-") shouldBe 48
        hash("pc=6") shouldBe 214
        hash("ot=7") shouldBe 231
      }
    }
  }
}