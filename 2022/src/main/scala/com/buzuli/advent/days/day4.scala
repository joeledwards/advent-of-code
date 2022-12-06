package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.immutable.SortedSet
import scala.concurrent.{ExecutionContext, Future}

object day4 extends AdventDay(4) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  def parseLine(line: String): Option[(SortedSet[Int], SortedSet[Int])] = line match {
    case s"${aStart}-${aEnd},${bStart}-${bEnd}" => for {
      as <- aStart.toIntOption
      ae <- aEnd.toIntOption
      bs <- bStart.toIntOption
      be <- bEnd.toIntOption
    } yield((SortedSet.from(Range(as, ae + 1)), SortedSet.from(Range(bs, be + 1))))
    
    case _ => None
  }
  
  val partnerships: List[(SortedSet[Int], SortedSet[Int])] = lines.flatMap(parseLine)
  
  object p1 {
    val fullyOverlapping: List[(SortedSet[Int], SortedSet[Int])] = {
      partnerships
        .flatMap({
          case pair @ (a, b) if a.subsetOf(b) || b.subsetOf(a) => Some(pair)
          case _ => None
        })
    }
  
    def answer: String = s"${fullyOverlapping.size}"
  }
  
  object p2 {
    val anyOverlap: List[(SortedSet[Int], SortedSet[Int])] = {
      partnerships
        .flatMap({
          case pair @ (a, b) if a.intersect(b).nonEmpty => Some(pair)
          case _ => None
        })
    }
    
    def answer: String = s"${anyOverlap.size}"
  }
}
