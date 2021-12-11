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
      fish
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

  val fish: List[Int] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(_.split(","))
      .map(_.toInt)
      .toList
  }
}
