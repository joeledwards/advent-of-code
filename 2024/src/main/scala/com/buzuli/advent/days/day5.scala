package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$sumMiddleOfCorrectUpdates"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$sumMiddleOfIncorrectUpdates"
  }

  def checkValid(rules: List[(Int, Int)], update: List[Int]): Either[List[Int], List[Int]] = {
    val ruleSet = rules.toSet
    def isLess(a: Int, b: Int): Boolean = ruleSet.contains(a -> b)
    val sorted = update.sortWith(isLess)

    if (sorted == update) {
      Right(update)
    } else {
      Left(sorted)
    }
  }

  sealed trait UpdateDisposition
  case object UpdateCorrect extends UpdateDisposition
  case object UpdateIncorrect extends UpdateDisposition

  def sumUpdates(updateDisposition: UpdateDisposition): Long = {
    val (ruleStrings, updateStrings) = lines.filter(_.nonEmpty).partition(_.matches("^\\d\\d\\|\\d\\d$"))
    val rules = ruleStrings.map { case s"${low}|${high}" => low.toInt -> high.toInt }
    val updates = updateStrings.map { l => l.split(",").map(_.toInt).toList }
    val validUpdates = updates.flatMap(update =>{
      updateDisposition match {
        case UpdateCorrect   => checkValid(rules, update).toOption
        case UpdateIncorrect => checkValid(rules, update).left.toOption
      }
    })

    validUpdates
      .map { update => update(update.length / 2) }
      .sum
  }

  def sumMiddleOfCorrectUpdates: Long = sumUpdates(UpdateCorrect)

  def sumMiddleOfIncorrectUpdates: Long = sumUpdates(UpdateIncorrect)
}
