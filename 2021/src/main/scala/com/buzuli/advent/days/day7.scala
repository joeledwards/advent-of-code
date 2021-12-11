package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day7 extends AdventDay(7) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
    //List(puzzle1)
    //List(puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (position, fuel) = resolveFuel(false, crabs)

    fuel.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (position, fuel) = resolveFuel(true, crabs)

    fuel.toString
  }

  def fuelConstant(align: Int, clusters: List[(Int, Int)]): Int = {
    clusters.map({ case (position, count) => Math.abs(align - position) * count }).sum
  }

  def fuelAdditive(align: Int, clusters: List[(Int, Int)]): Int = {
    clusters.map({ case (position, count) =>
     val offset = Math.abs(align - position)
     offset match {
       case 0 => 0
       case n => (((n + 1) / 2.0) * n).toInt * count
     }
    }).sum
  }

  def split(clusters: List[(Int, Int)]): (List[(Int, Int)], List[(Int, Int)]) = {
    clusters match {
      case Nil => (Nil, Nil)
      case item :: Nil => (List(item), Nil)
      case items => items.splitAt(items.length / 2)
    }
  }

  def resolveFuel(additive: Boolean, crabs: List[Int]): (Int, Int) = {
    val calculateFuel: (Int, List[(Int, Int)]) => Int = {
      if (additive) fuelAdditive else fuelConstant
    }

    val count = crabs.length
    val mean = crabs.sum / count
    val median = crabs.sorted.apply(count / 2)

    val clusters: List[(Int, Int)] = {
      crabs
        .groupBy(x => x)
        .map({ case (k, v) => (k, v.length) })
        .toList
        .sorted
    }
    val min = clusters.head._1
    val max = clusters.last._1

    val positionRange = Range(min, max+1)

    var minimum: Option[(Int, Int)] = None

    for (position <- positionRange) {
      val fuel = calculateFuel(position, clusters)
      minimum = {
        minimum
          .map({ old => if (old._2 < fuel) old else (position, fuel) })
          .orElse(Some((position, fuel)))
      }
    }

    minimum.get
  }

  val crabs: List[Int] = {
    lines
      .flatMap { line =>
        line.split(",")
          .view
          .map(_.toInt)
          .toList
      }
  }
}
