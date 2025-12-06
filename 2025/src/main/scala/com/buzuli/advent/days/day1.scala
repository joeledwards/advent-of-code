package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day1 extends AdventDay(1) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${totalDistance}"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${similarityScore}"
  }

  def totalDistance: Long = {
    val idPairs = lines.map { case s"${a}   ${b}" => a.toLong -> b.toLong }
    val (a, b) = idPairs.unzip
    val idOrderedPairs = a.sorted zip b.sorted
    val idDiffs = idOrderedPairs map { case (a, b) => Math.abs(a - b) }
    idDiffs.sum
  }

  def similarityScore: Long = {
    val idPairs = lines.map { case s"${a}   ${b}" => a.toLong -> b.toLong }
    val (a, b) = idPairs.unzip
    val valueCounts = b.map(_ -> 1L).groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sum } toMap
    val scores = a.map { v => valueCounts.getOrElse(v, 0L) * v }
    scores.sum
  }
}
