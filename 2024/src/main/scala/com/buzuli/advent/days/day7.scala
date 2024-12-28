package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day7 extends AdventDay(7) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$sumValid"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$sumValidWithConcat"
  }

  def sumValid: Long = {
    case class Equation(
      answer: Long,
      values: List[Long],
    )

    def equations: List[Equation] = {
      lines
        .flatMap {
          case s"$answerStr: $restStr" => {
            Some(Equation(
              answer = answerStr.toLong,
              values = restStr.split(" ").toList.map(_.toLong).reverse,
            ))
          }
          case _ => None
        }
    }

    def testEquation(equation: Equation): Boolean = {
      equation match {
        case Equation(_, Nil) => false
        case Equation(a, b :: Nil) if a == b => true
        case Equation(_, _ :: Nil) => false
        case Equation(a, b :: l) => {
          (
            testEquation(Equation(a - b, l))
          ) || (
            (a % b == 0) && testEquation(Equation(a / b, l))
          )
        }
      }
    }

    equations.filter(testEquation).map(_.answer).sum
  }

  def sumValidWithConcat: Long = {
    case class Equation(
      answer: Long,
      values: List[Long],
    )

    def equations: List[Equation] = {
      lines
        .flatMap {
          case s"$answerStr: $restStr" => {
            Some(Equation(
              answer = answerStr.toLong,
              values = restStr.split(" ").toList.map(_.toLong),
            ))
          }
          case _ => None
        }
    }

    def testEquation(equation: Equation, acc: Option[Long] = None): Boolean = {
      (equation, acc) match {
        case (Equation(_, Nil), None) => false
        case (Equation(a, Nil), Some(v)) => a == v
        case (Equation(a, b :: l), None) => testEquation(Equation(a, l), Some(b))
        case (Equation(a, b :: l), Some(v)) => {
          (
            testEquation(Equation(a, l), Some(v + b))
            ) || (
            testEquation(Equation(a, l), Some(v * b))
            ) || (
            testEquation(Equation(a, l), Some(s"$v$b".toLong))
            )
        }
      }
    }

    equations.filter(testEquation(_)).map(_.answer).sum
  }
}
