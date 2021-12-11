package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day10 extends AdventDay(10) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  sealed trait ParseResult
  case class ParseSuccess(score: Int) extends ParseResult
  case object ParseInvalid extends ParseResult
  case object ParseIncomplete extends ParseResult

  def closing(c: Char): Char = c match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _ => ' '
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    def points(c: Char): Int = c match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
      case _ => 0
    }

    val scores = data map { line =>
      var stack: List[Char] = Nil

      line.toList.foldLeft[Option[Int]](None) { case (acc, c) =>
        acc.orElse {
          c match {
            case '(' | '[' | '{' | '<' => {
              stack = closing(c) :: stack
              None
            }
            case ')' | ']' | '}' | '>' => {
              val head = stack.head
              stack = stack.tail
              if (c == head) {
                None
              } else {
                val score = points(c)
                Some(score)
              }
            }
            case _ => throw new Exception(s"Invalid character '${c}''")
          }
        }
      } orElse { Some(0) }
    }

    val score = scores.foldLeft[Int](0) {
      case (acc, None) => acc
      case (acc, Some(v)) => acc + v
    }

    score.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    def points(c: Char): Long = c match {
      case ')' => 1L
      case ']' => 2L
      case '}' => 3L
      case '>' => 4L
      case _ => 0L
    }

    val incompletes = data map { line =>
      var stack: List[Char] = Nil

      line.toList.takeWhile {
        case c@('(' | '[' | '{' | '<') => {
          stack = closing(c) :: stack
          true
        }
        case c@(')' | ']' | '}' | '>') => {
          val head = stack.head
          stack = stack.tail
          if (c == head) {
            true
          } else {
            stack = Nil
            false
          }
        }
        case c => throw new Exception(s"Invalid character '${c}''")
      }

      stack
    } filter { _.nonEmpty }

    val scores = incompletes.map({ missing =>
      val score = missing.map(points).foldLeft(0L) {
        (acc, score) => acc * 5L + score
      }
      score
    }).sorted

    val score = scores(scores.length / 2)

    score.toString
  }

  lazy val data: List[String] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
  }

  lazy val sample: String = {
    """
      |[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |""".stripMargin
  }
}
