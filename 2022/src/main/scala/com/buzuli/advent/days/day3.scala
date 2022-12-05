package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day3 extends AdventDay(3) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  case class RuckSack(a: Set[Char], b: Set[Char]) {
    val commonElement: Option[Item] = {
      val intersect = a.intersect(b)
      assert(intersect.size < 2)
      intersect.headOption.map(Item(_))
    }
  }
  
  object RuckSack {
    def fromString(str: String): Option[RuckSack] = str match {
      case s if s.length < 2 => None
      case s if s.length % 2 != 0 => None
      case s => {
        val (a, b) = s.splitAt(s.length / 2)
        Some(RuckSack(a.toCharArray.toSet, b.toCharArray.toSet))
      }
    }
  }
  
  case class Item(v: Char) {
    val priority = {
      val codePoint = v.toInt
      if (codePoint > 64 && codePoint < 91)
        codePoint - 38 // scores 27 - 52 (A-Z)
      else
        codePoint - 96 // scores 1 - 26 (a-z)
    }
  }
  
  object p1 {
    val rucksacks: List[RuckSack] = lines.flatMap(RuckSack.fromString(_).toList)
    val priorities: List[Int] = rucksacks.map(_.commonElement.map(_.priority).getOrElse(0))
    def answer: String = s"${priorities.sum}"
  }
  
  object p2 {
    val rucksacks: List[RuckSack] = lines.flatMap(RuckSack.fromString(_).toList)
    val groupPriorities: List[Int] = rucksacks
      .grouped(3)
      .flatMap({
        case List (e1, e2, e3) => ((e1.a | e1.b) & (e2.a | e2.b) & (e3.a | e3.b) ).headOption.map(Item)
        case _ => None
      })
      .map(_.priority)
      .toList
  
    def answer: String = s"${groupPriorities.sum}"
  }
}
