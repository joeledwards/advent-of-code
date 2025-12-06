package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

case class Displacement(delta: Long)

object Displacement {
  def apply(text: String): Option[Displacement] = text match {
    case s"R${value}" => Try(value.toLong).toOption.map(Displacement(_))
    case s"L${value}" => Try(value.toLong).toOption.map(v => Displacement(-1L * v))
    case _ => None
  }
}

case class Dial(offset: Long, zeros: Long) {
  def rotate(delta: Long): Dial = {
    val change = delta % 100
    val adjusted = offset + change
    val corrected = {
      if (adjusted < 0L) {
        adjusted + 100
      } else if (adjusted > 99) {
        adjusted - 100
      } else {
        adjusted
      }
    }

    Dial(corrected, if (corrected == 0) zeros + 1 else zeros)
  }

  def rotate(displacement: Displacement): Dial = rotate(displacement.delta)
}

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${zeroCount}"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    //s"${similarityScore}"
    ""
  }

  def zeroCount: Long = {
    val displacements = lines.flatMap(Displacement(_))
    val updated = displacements.foldLeft(Dial(50, 0))(_.rotate(_))
    updated.zeros
  }
}
