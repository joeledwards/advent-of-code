package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

object day11 extends AdventDay(11) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    // Sample flashes after 100 steps => 1656
    val (score, _) = calculateStepFlashes(100)
    score.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (_, syncStep) = calculateStepFlashes(100)
    syncStep.toString
  }

  def calculateStepFlashes(stepsToCount: Int): (Int, Option[Int]) = {
    val all: Map[(Int, Int), Octopus] = {
      newGrid.toList.zipWithIndex.flatMap({ case (rows, y) =>
        rows.toList.zipWithIndex.map({ case (v, x) =>
          ((x, y), Octopus(x, y, v))
        })
      }).toMap
    }

    def incrementAndCollect(octopi: Set[Octopus]): Set[Octopus] = {
      octopi.map({ o =>
        if (o.increment()) {
          Some(o)
        } else {
          None
        }
      }) collect {
        case Some(Octopus(x, y, _)) => all((x, y))
      }
    }

    var step = 1
    var score = 0
    var syncStep: Option[Int] = None

    while (syncStep.isEmpty || step <= stepsToCount) { // steps
      // Increment values and identify flashers
      var flashers = incrementAndCollect(all.values.toSet)
      var stepFlashes = 0

      // Apply flashes while there are more that need to emit a flash
      while (flashers.nonEmpty) {
        flashers = flashers.flatMap { o =>
          stepFlashes += 1
          incrementAndCollect(o.flash().map(all))
        }
      }

      // Reset any which flashed
      all.values.foreach(_.reset())

      if (stepFlashes == 100 && syncStep.isEmpty) {
        syncStep = Some(step)
      }

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
    v: Int
  ) {
    var value: Int = v
    var flashCount: Int = 0

    /**
     * Reset the value and whether it flashed.
     */
    def reset(): Unit = {
      if (value > 9) {
        value = 0
      }
    }

    /**
     * Indicate which
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
      value += 1
      value == 10
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
