package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

case class Displacement(delta: Long)

object Displacement {
  def apply(text: String): Option[Displacement] = text match {
    case s"R${value}" => Try(value.toLong).toOption.map(Displacement(_))
    case s"L${value}" => Try(value.toLong).toOption.map(v => Displacement(-1L * v))
    case _ => None
  }
}

case class Dial(offset: Long, stopZeros: Long, clickZeros: Long) {
  def rotate(delta: Long): Dial = {
    val fullPasses = Math.abs(delta) / 100
    val change = delta % 100
    val adjusted = offset + change

    val updatedOffset = {
      if (adjusted < 0L) {
        adjusted + 100
      } else if (adjusted > 99) {
        adjusted - 100
      } else {
        adjusted
      }
    }

    val updatedStopZeros = {
      if (updatedOffset == 0)
        stopZeros + 1
      else
        stopZeros
    }

    val updatedClickZeros = {
      val extraZeroLand = {
        if (offset == 0)
          0
        else if (change != 0 && updatedOffset == 0)
          1
        else if (change > 0 && updatedOffset < offset)
          1
        else if (change < 0 && updatedOffset > offset)
          1
        else
          0
      }

      clickZeros + fullPasses + extraZeroLand
    }

    Dial(updatedOffset, updatedStopZeros, updatedClickZeros)
  }

  def rotate(displacement: Displacement): Dial = rotate(displacement.delta)
}

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${rotationsPassingZero}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${totalZeroPasses}"
  }

  def rotationsPassingZero: Long = {
    val displacements = lines.flatMap(Displacement(_))
    val updated = displacements.foldLeft(Dial(50, 0, 0))(_.rotate(_))
    updated.stopZeros
  }

  def totalZeroPasses: Long = {
    val displacements = lines.flatMap(Displacement(_))
    val updated = displacements.foldLeft(Dial(50, 0, 0))(_.rotate(_))
    updated.clickZeros
  }
}
