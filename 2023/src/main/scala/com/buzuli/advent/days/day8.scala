package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day8 extends AdventDay(8) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1StepCount}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2ParallelNav}"
  }

  lazy val puzzle1StepCount: Long = {
    var count = 0L
    var id = "AAA"

    while (id != "ZZZ") {
      directions foreach { d =>
        id match {
          case "ZZZ" =>
          case key => {
            count += 1
            id = d match {
              case Left => nodes(key).left
              case Right => nodes(key).right
            }
          }
        }
      }
    }

    count
  }

  lazy val puzzle2ParallelNav: Long = {
    var count = 0L
    var ids: List[String] = nodes.toList.map(_._1).filter(_.endsWith("A"))

    println(s"Starting IDs: ${ids}")

    def notDone: Boolean = ids.exists(!_.endsWith("Z"))

    while (notDone) {
      directions foreach { d =>
        if (notDone) {
          count += 1
          ids = ids map { id =>
            d match {
              case Left => nodes(id).left
              case Right => nodes(id).right
            }
          }
        }
      }
    }

    count
  }

  lazy val directions: List[Direction] = {
    lines
      .head
      .view
      .flatMap(Direction.fromChar)
      .toList
  }

  lazy val nodes: Map[String, Node] = {
    lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .collect({
        case s"${id} = (${left}, ${right})" => Node(id, left, right)
      })
      .map(n => (n.id, n))
      .toMap
  }

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  object Direction {
    def fromChar(c: Char): Option[Direction] = c match {
      case 'L' => Some(Left)
      case 'R' => Some(Right)
      case _ => None
    }
  }

  case class Node(
    id: String,
    left: String,
    right: String,
  )
}
