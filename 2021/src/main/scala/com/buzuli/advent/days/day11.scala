package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day11 extends AdventDay(11) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (score, _) = calculateStepFlashes(100)
    score.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (_, syncStep) = calculateStepFlashes(100)
    syncStep.toString
  }

  def calculateStepFlashes(stepsToCount: Int): (Int, Option[Int]) = {
    // The map where we can lookup an octopus by its coordinates
    // (needed in order to fetch neighboring octopi)
    val all: Map[(Int, Int), Octopus] = {
      newGrid.toList.zipWithIndex.flatMap({ case (rows, y) =>
        rows.toList.zipWithIndex.map({ case (v, x) =>
          ((x, y), Octopus(x, y, v))
        })
      }).toMap
    }

    // Increment the power for all octopi in the set, then return all
    // which reached at the flash threshold.
    def incrementAndCollect(octopi: Set[Octopus]): Set[Octopus] = {
      octopi.map({ o =>
        Option.when(o.increment())(o)
      }) collect {
        case Some(Octopus(x, y, _)) => all((x, y))
      }
    }

    var step = 1
    var score = 0
    var syncStep: Option[Int] = None

    while (syncStep.isEmpty || step <= stepsToCount) { // steps
      // Increment values and identify initial flashers
      var flashers = incrementAndCollect(all.values.toSet)
      var stepFlashes = 0

      // Apply flashes, then increment those adjacent to the flashers, and any
      // which met their flash threshold become part of the next set of flashers
      while (flashers.nonEmpty) {
        flashers = flashers.flatMap { o =>
          stepFlashes += 1
          incrementAndCollect(o.flash().map(all))
        }
      }

      // Reset any which flashed
      all.values.foreach(_.reset())

      // Record the first case where all octopi flashed during the same step
      if (stepFlashes == 100 && syncStep.isEmpty) {
        syncStep = Some(step)
      }

      // Save the score (flash count) if we reached the number of target steps
      if (step == stepsToCount) {
        score = all.values.map(_.flashCount).sum
      }

      step += 1
    }

    (score, syncStep)
  }

  case class Octopus(
    x: Int,
    y: Int,
    initialPower: Int
  ) {
    var power: Int = initialPower
    var flashCount: Int = 0

    /**
     * Reset the power if the octopus flashed.
     */
    def reset(): Unit = {
      if (power > 9) {
        power = 0
      }
    }

    /**
     * Supply all neighboring octopi.
     */
    def flash(): Set[(Int, Int)] = {
      flashCount += 1
      Set(
        (x - 1, y),
        (x + 1, y),
        (x, y - 1),
        (x, y + 1),
        (x - 1, y - 1),
        (x - 1, y + 1),
        (x + 1, y - 1),
        (x + 1, y + 1),
      ) filter {
        case (x, _) if x < 0 => false
        case (x, _) if x > 9 => false
        case (_, y) if y < 0 => false
        case (_, y) if y > 9 => false
        case _ => true
      }
    }

    /**
     * Increment the internal value, and indicate whether a flash should be emitted.
     */
    def increment(): Boolean = {
      power += 1
      power == 10
    }
  }

  def newGrid: Array[Array[Int]] = {
    lines
      .view
      .filter(_.nonEmpty)
      .map( line =>
        line
          .view
          .map(_.toInt - 48)
          .toArray
      )
      .toArray
  }

  lazy val sample: List[String] = {
    """
      |5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526
      |""".stripMargin.split("\n").toList
  }
}
