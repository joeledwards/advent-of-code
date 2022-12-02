package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${maxCalories}"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${topNCalories(3)}"
  }
  
  def maxCalories = {
    elfCalories.max
  }
  
  def topNCalories(n: Int): Int = {
    elfCalories.sorted.reverse.take(n).sum
  }
  
  val elfCalories: List[Int] = {
    lines
      .view
      .map(_.trim)
      .map {
        case "" => None
        case c => Some(c.toInt)
      }
      .foldLeft[List[Option[Int]]](Nil) { (acc: List[Option[Int]], snack: Option[Int]) =>
        (acc, snack) match {
          case (Nil, None) => None :: Nil
          case (Nil, Some(v)) => Some(v) :: Nil
          case (None :: rest, None) => None :: rest
          case (None :: rest, Some(v)) => Some(v) :: rest
          case (Some(c) :: rest, None) => None :: Some(c) :: rest
          case (Some(c) :: rest, Some(v)) => Some(c + v) :: rest
        }
      }
      .collect {
        case Some(v) => v
      }
      .toList
  }
}
