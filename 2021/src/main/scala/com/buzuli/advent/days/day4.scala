package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object day4 extends AdventDay(4) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    var result: Option[Int] = None
    val boards = newBoards

    selections.takeWhile { value =>
      boards.takeWhile { board =>
        result = board.mark(value)
        result.isEmpty
      }
      result.isEmpty
    }

    result.getOrElse(0).toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    var result: Option[Int] = None
    var boards = newBoards

    selections.takeWhile { value =>
      boards = boards.filter { board =>
        board.mark(value) match {
          case None => true
          case Some(score) => {
            result = Some(score)
            false
          }
        }
      }
      boards.nonEmpty
    }

    result.getOrElse(0).toString
  }

  val (_selections: String, _boards: List[List[String]]) = {
    lines match {
      case s :: b => (
        s,
        b.filter(_.trim.nonEmpty).grouped(6).toList
      )
    }
  }

    val selections: List[Int] = {
      _selections
        .split(",")
        .map(_.toInt)
        .toList
    }

    def newBoards: List[Board] = _boards.map { Board }

    case class Board(
      boardLines: List[String],
    ) {
      val matrix: List[List[Int]] = {
        boardLines
          .view
          .map(_.trim)
          .filter(_.nonEmpty)
          .map { line =>
            line
              .split(" ")
              .view
              .map(_.trim)
              .filter(_.nonEmpty)
              .map(_.toInt)
              .toList
          }.toList
      }

      val width: Int = matrix.length
      val height: Int = matrix.head.length

      // Map the value back to the coordinates
      val valueToCoordinateMap: mutable.Map[Int, (Int, Int)] = {
        mutable.Map.from(
          matrix.zipWithIndex.flatMap({ case (subList, y) =>
            subList.zipWithIndex.map({ case (value, x) =>
              (value, (x, y))
            })
          })
        )
      }

      var colCounts: mutable.Map[Int, Int] = mutable.Map()
      var rowCounts: mutable.Map[Int, Int] = mutable.Map()

      /**
       * Mark the position on the board, indicating a bingo by returning the score.
       *
       * @param value the value to mark
       *
       * @return Some(score) if marking this value resulted in a bingo
       */
      def mark(value: Int): Option[Int] = {
        valueToCoordinateMap.remove(value) flatMap { case (x, y) =>
          val xBingo = colCounts
            .updateWith(x)(_.orElse(Some(0)).map(_+1))
            .exists(_ >= width)

          val yBingo = rowCounts
            .updateWith(y)(_.orElse(Some(0)).map(_+1))
            .exists(_ >= height)

          val bingo = xBingo || yBingo

          Option.when(bingo)(valueToCoordinateMap.keys.sum * value)
        }
      }
    }
}
