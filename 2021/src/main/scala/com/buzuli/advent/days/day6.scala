package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object day6 extends AdventDay(6) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    generation(80).toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    generation(256).toString
  }

  def generation(numGenerations: Int): Long = {
    var fishMap: mutable.Map[Int, Long] = {
      val map = mutable.Map[Int, Long]()
      data
        .fish
        .groupBy(x => x)
        .foreach { case (k, l) =>
          map.put(k, l.count(_ => true))
        }
      map
    }

    for (_ <- Range(0, numGenerations)) {
      val nextMap = mutable.Map[Int, Long]()
      fishMap.foreach {
        case (0, count) => {
          nextMap.updateWith(6)(_.orElse(Some(0L)).map(_+count))
          nextMap.updateWith(8)(_.orElse(Some(0L)).map(_+count))
        }
        case (n, count) => {
          nextMap.updateWith(n - 1)(_.orElse(Some(0L)).map(_+count))
        }
      }
      fishMap = nextMap
    }

    fishMap.values.sum
  }

  object data {
    val fish: List[Int] = {
      raw
        .split("\n")
        .view
        .map(_.trim)
        .filter(_.nonEmpty)
        .flatMap(_.split(","))
        .map(_.toInt)
        .toList
    }

    lazy val raw: String =
      """
        |1,1,1,1,2,1,1,4,1,4,3,1,1,1,1,1,1,1,1,4,1,3,1,1,1,5,1,3,1,4,1,2,1,1,5,1,1,1,1,1,1,1,1,1,1,3,4,1,5,1,1,1,1,1,1,1,1,1,3,1,4,1,1,1,1,3,5,1,1,2,1,1,1,1,4,4,1,1,1,4,1,1,4,2,4,4,5,1,1,1,1,2,3,1,1,4,1,5,1,1,1,3,1,1,1,1,5,5,1,2,2,2,2,1,1,2,1,1,1,1,1,3,1,1,1,2,3,1,5,1,1,1,2,2,1,1,1,1,1,3,2,1,1,1,4,3,1,1,4,1,5,4,1,4,1,1,1,1,1,1,1,1,1,1,2,2,4,5,1,1,1,1,5,4,1,3,1,1,1,1,4,3,3,3,1,2,3,1,1,1,1,1,1,1,1,2,1,1,1,5,1,3,1,4,3,1,3,1,5,1,1,1,1,3,1,5,1,2,4,1,1,4,1,4,4,2,1,2,1,3,3,1,4,4,1,1,3,4,1,1,1,2,5,2,5,1,1,1,4,1,1,1,1,1,1,3,1,5,1,2,1,1,1,1,1,4,4,1,1,1,5,1,1,5,1,2,1,5,1,1,1,1,1,1,1,1,1,1,1,1,3,2,4,1,1,2,1,1,3,2
        |""".stripMargin
  }
}
