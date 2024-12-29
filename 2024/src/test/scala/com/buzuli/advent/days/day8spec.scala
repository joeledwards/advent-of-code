package com.buzuli.advent.days

import com.buzuli.advent.days.day8._
import com.buzuli.UnitSpec

class day8spec extends UnitSpec {
  "Day8.Point" when {
    "offset()" should {
      "compute the expected offset" in {
        Point(1, 1).offset(Point(3, 3)) shouldBe Offset(2, 2)
        Point(3, 3).offset(Point(1, 1)) shouldBe Offset(-2, -2)
      }
    }

    "+" should {
      "generated the expected point" in {
        Point(1, 1) + Offset(-1, -1) shouldBe Point(0, 0)
        Point(1, 1) + Offset(-1,  1) shouldBe Point(0, 2)
        Point(1, 1) + Offset( 1, -1) shouldBe Point(2, 0)
        Point(1, 1) + Offset( 1,  1) shouldBe Point(2, 2)
      }
    }

    "-" should {
      "generated the expected point" in {
        Point(1, 1) - Offset(-1, -1) shouldBe Point(2, 2)
        Point(1, 1) - Offset(-1,  1) shouldBe Point(2, 0)
        Point(1, 1) - Offset( 1, -1) shouldBe Point(0, 2)
        Point(1, 1) - Offset( 1,  1) shouldBe Point(0, 0)
      }
    }
  }

  "Day8.Grid" when {
    "contains()" should {
      "correctly identify contained points" in {
        val grid = Grid(Point(2, 2), Point(4, 4))

        grid.contains(Point(2,2)) shouldBe true
        grid.contains(Point(4,4)) shouldBe true
        grid.contains(Point(2,4)) shouldBe true
        grid.contains(Point(4,2)) shouldBe true
        grid.contains(Point(3,3)) shouldBe true

        grid.contains(Point(1,1)) shouldBe false
        grid.contains(Point(1,5)) shouldBe false
        grid.contains(Point(5,1)) shouldBe false
        grid.contains(Point(5,5)) shouldBe false
        grid.contains(Point(4,1)) shouldBe false
        grid.contains(Point(1,4)) shouldBe false
        grid.contains(Point(3,1)) shouldBe false
        grid.contains(Point(1,3)) shouldBe false
      }
    }
  }

  "Day8.findAntinodesForAntennaPair(Grid, Boolean)(Antenna, Antenna)" when {
    "locating antinodes in the grid" should {
      "locate all valid antinodes" in {
        def test(a: Point, b: Point)(expected: Set[Point]): Unit = {
          val grid = Grid(Point(0, 0), Point(10, 10))
          val antA = Antenna(a, "A")
          val antB = Antenna(b, "A")
          val antinodes = findAntinodesForAntennaPair(grid)(antA, antB)
          val actual = antinodes.map(_.location).toSet

          actual shouldBe expected
        }

        test(Point(3, 3), Point(7, 7))(Set())
        test(Point(7, 7), Point(3, 3))(Set())

        test(Point(0, 0), Point(1, 1))(Set(Point(2, 2)))
        test(Point(1, 1), Point(0, 0))(Set(Point(2, 2)))

        test(Point(2, 2), Point(3, 3))(Set(Point(1, 1), Point(4, 4)))
        test(Point(3, 3), Point(2, 2))(Set(Point(1, 1), Point(4, 4)))

        test(Point(2, 2), Point(1, 3))(Set(Point(0, 4), Point(3, 1)))
        test(Point(1, 3), Point(2, 2))(Set(Point(0, 4), Point(3, 1)))

        test(Point(2, 2), Point(3, 1))(Set(Point(4, 0), Point(1, 3)))
        test(Point(3, 1), Point(2, 2))(Set(Point(4, 0), Point(1, 3)))
      }
    }
  }
}
