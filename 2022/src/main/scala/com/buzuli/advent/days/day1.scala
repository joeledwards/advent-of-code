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
        // Empty lines are the delimiters between the groups of snacks carried by each elf.
        case "" => None
        // Populated lines are the snacks (calorie count for the snack)
        case c => Some(c.toInt)
      }
      .foldLeft[List[Option[Int]]](Nil) { (acc: List[Option[Int]], snack: Option[Int]) =>
        // A fold left makes sense because we want to accumulate the calorie count for each elf in turn,
        // but we don't know how many snacks each elf will be carrying or how many spaces there are between elves.
        (acc, snack) match {
          // First line is empty. We have one elf with no snacks.
          case (Nil, None) => None :: Nil
          // First line is populated. We have one elf with a calorie count. Could be zero (who brings celery?).
          case (Nil, Some(v)) => Some(v) :: Nil
          // The current elf has no snacks, and we are at a gap, so we don't bother advancing.
          case (None :: rest, None) => None :: rest
          // The current elf has no snacks, and we are processing a snack, so we initialize the elf's calorie count.
          case (None :: rest, Some(v)) => Some(v) :: rest
          // The current elf has snacks, and we are at a gap, so we add a new elf with no snacks.
          case (Some(c) :: rest, None) => None :: Some(c) :: rest
          // The current elf has snacks, and we are processing a snack, so we add to the current elf's calorie count.
          case (Some(c) :: rest, Some(v)) => Some(c + v) :: rest
        }
      }
      .collect {
        // Now that we have accumulated all each elf's calorie count, convert to a simple list of calorie counts.
        case Some(v) => v
      }
  }
}
