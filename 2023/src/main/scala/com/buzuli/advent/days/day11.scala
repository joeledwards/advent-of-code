package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day11 extends AdventDay(11) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$puzzle1ExpanseSums"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$puzzle2MassExpanseSums"
  }

  lazy val puzzle1ExpanseSums: Long = {
    val expandedUniverse = expanded(universe)

    //printUniverse(universe)
    //printUniverse(expandedUniverse)

    def collectMinDistances(galaxies: List[Galaxy]): List[Long] = {
      galaxies match {
        case Nil => Nil
        case _ :: Nil => Nil
        case g :: rest => {
          rest.map(o => Math.abs(g.x - o.x) + Math.abs(g.y - o.y)) ::: collectMinDistances(rest)
        }
      }
    }

    collectMinDistances(expandedUniverse).sum
  }

  def printUniverse(universe: List[Galaxy]): Unit = {
    val uniMap = universe
      .map(g => g.x -> g.y -> g)
      .toMap

    val xMax = universe.map(_.x).max
    val yMax = universe.map(_.y).max

    println({
      Range.inclusive(0, yMax.toInt).map(_.toLong).map({ y =>
        Range.inclusive(0, xMax.toInt).map(_.toLong).map({ x =>
          uniMap.get(x -> y) match {
            case None => "."
            case Some(Galaxy(id, _, _)) => s"${id}"
            //case Some(Galaxy(_, _, _)) => "#"
          }
        }
        ).mkString("")
      }
      ).mkString("\n")
    }
    )
  }

  lazy val puzzle2MassExpanseSums: Long = {
    val expandedUniverse = expanded(universe, 999999)

    def collectMinDistances(galaxies: List[Galaxy]): List[Long] = {
      galaxies match {
        case Nil => Nil
        case _ :: Nil => Nil
        case g :: rest => {
          rest.map(o => Math.abs(g.x - o.x) + Math.abs(g.y - o.y)) ::: collectMinDistances(rest)
        }
      }
    }

    collectMinDistances(expandedUniverse).sum
  }

  def expanded(
    galaxies: List[Galaxy],
    expansion: Long = 1
  ): List[Galaxy] = {
    var expandedUniverse = galaxies

    var xEmptyCount: Long = 0
    var xPrev: Option[Long] = None
    val xOrdering: List[Galaxy] = expandedUniverse.sortBy(_.x)

    expandedUniverse = xOrdering.map({ g =>
      xEmptyCount = xEmptyCount + {
        xPrev match {
          case None => g.x * expansion
          case Some(x) if (g.x - x) == 0 => 0
          case Some(x) if (g.x - x) == 1 => 0
          case Some(x) => (g.x - x - 1) * expansion
        }
      }

      xPrev = Some(g.x)

      g.copy(x = g.x + xEmptyCount)
    })

    var yEmptyCount: Long = 0
    var yPrev: Option[Long] = None
    val yOrdering: List[Galaxy] = expandedUniverse.sortBy(_.y)

    expandedUniverse = yOrdering.map({ g =>
      yEmptyCount = yEmptyCount + {
        yPrev match {
          case None => g.y * expansion
          case Some(y) if (g.y - y) == 0 => 0
          case Some(y) if (g.y - y) == 1 => 0
          case Some(y) => (g.y - y - 1) * expansion
        }
      }

      yPrev = Some(g.y)

      g.copy(y = g.y + yEmptyCount)
    })

    expandedUniverse.sortBy(g => g.x -> g.y)
  }

  lazy val universe: List[Galaxy] = {
    var _nextId: Long = 0
    def nextId: Long = {
      _nextId += 1
      _nextId
    }

    lines
      .zipWithIndex
      .flatMap({ case (line, y) =>
        line
          .view
          .zipWithIndex
          .flatMap({
            case ('#', x) => Some(Galaxy(nextId, x, y))
            case _ => None
          })
          .toList
      })
  }

  case class Galaxy(
    id: Long,
    x: Long,
    y: Long
  )
}