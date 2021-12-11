package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day2 extends AdventDay(2) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (depth, distance) = commands.foldLeft((0, 0)) { (acc, command) =>
      val (depth, distance) = acc
      command match {
        case s"up ${value}" => (depth - value.toInt, distance)
        case s"down ${value}" => (depth + value.toInt, distance)
        case s"forward ${value}" => (depth, distance + value.toInt)
      }
    }

    (depth * distance).toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (depth, _, distance) = commands.foldLeft((0, 0, 0)) { (acc, command) =>
      val (depth, aim, distance) = acc
      command match {
        case s"up ${value}" => (depth, aim - value.toInt, distance)
        case s"down ${value}" => (depth, aim + value.toInt, distance)
        case s"forward ${value}" => (depth + aim * value.toInt, aim, distance + value.toInt)
      }
    }

    (depth * distance).toString
  }

  val commands: List[String] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
  }
}
