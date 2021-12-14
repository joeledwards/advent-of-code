package com.buzuli.advent.days

import com.buzuli.advent.days.day13.marks
import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day13 extends AdventDay(13) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    applyFold(folds.head)(marks).size.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val updatedMarks = folds.foldLeft(marks) { (marks, fold) =>
      applyFold(fold)(marks)
    }

    val xMin = updatedMarks.map(_._1).min
    val xMax = updatedMarks.map(_._1).max
    val yMin = updatedMarks.map(_._2).min
    val yMax = updatedMarks.map(_._2).max

    val sb = new StringBuilder
    for (y <- yMin to yMax) {
      sb.addOne('\n')
      for (x <- xMin to xMax) {
        if (updatedMarks.contains((x, y))) {
          sb.addOne('|')
          sb.addOne('|')
        } else {
          sb.addOne(' ')
          sb.addOne(' ')
        }
      }
    }

    logger.info(s"${sb.toString}")

    "check-the-logs"
  }

  def applyFold(line: Either[Int, Int])(marks: Set[(Int, Int)]): Set[(Int, Int)] = line match {
    case Left(line) => {
      marks
        .map {
          case (x, y) if x > line => Some((line - (x - line), y))
          case (x, y) if x < line => Some((x, y))
          case _ => None
        }
        .collect {
          case Some(c) => c
        }
    }
    case Right(line) => {
      marks
        .map {
          case (x, y) if y > line => Some((x, line - (y - line)))
          case (x, y) if y < line => Some((x, y))
          case _ => None
        }
        .collect {
          case Some(c) => c
        }
    }
  }

  val marks: Set[(Int, Int)] = {
    lines
      .takeWhile(_.nonEmpty)
      .map {
        case s"${x},${y}" => (x.toInt, y.toInt)
      }
      .toSet
  }

  val folds: List[Either[Int, Int]] = {
    lines
      .reverse
      .takeWhile(_.nonEmpty)
      .reverse
      .map {
        case s"fold along x=${x}" => Left(x.toInt)
        case s"fold along y=${y}" => Right(y.toInt)
      }
  }

  lazy val sample: List[String] = {
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |""".stripMargin.split("\n").toList
  }
}