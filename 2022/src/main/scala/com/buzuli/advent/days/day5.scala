package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.immutable.SortedSet
import scala.concurrent.{ExecutionContext, Future}

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  case class Move(
    quantity: Int,
    from: Int,
    to: Int
  )
  
  def parseMove(line: String): Option[Move] = line match {
    case s"move ${quantity} from ${from} to ${to}" => for {
      q <- quantity.toIntOption
      f <- from.toIntOption
      t <- to.toIntOption
    } yield Move(q, f, t)
    case _ => None
  }
  
  def parseStacks(lines: List[String]): Map[Int, List[Char]] = {
    val baseMap = lines.foldLeft(Map.empty[Int, List[Char]])((acc, line) =>
      line.toCharArray.zipWithIndex.foldLeft(acc)({ (map, pair) =>
        val (char, index) = pair
        map + (index -> (char :: map.getOrElse(index, Nil)))
      })
    )
    
    val stackMap = baseMap.flatMap({
      case (_, index :: stack) => index.toString.toIntOption.map(i => (i, stack.reverse.filter(_ != ' ')))
      case _ => None
    })
    
    stackMap
  }
  
  val (
    stacks: Map[Int, List[Char]],
    moves: List[Move]
  ) = {
    val (stackLines, moveLines) = lines.partition(!_.startsWith("move"))
  
    (
      parseStacks(stackLines),
      moveLines.flatMap(parseMove)
    )
  }
  
  object p1 {
    val finalTopCrates: List[Char] = {
      val result = moves
        .foldLeft(stacks)({ (map, move) =>
          val Move(quantity, from, to) = move
          val (moved, retained) = {
            val toSplit = map(from)
            if (toSplit.length > quantity)
              toSplit.splitAt(quantity)
            else
              (toSplit, Nil)
          }
          val updatedMap = map + (from -> retained) + (to -> (moved.reverse ::: map(to)))
          updatedMap
        })
        .toList
        .sortBy(_._1)
        .flatMap(_._2.headOption)
    
      result
    }
  
    def answer: String = s"${finalTopCrates.mkString("")}"
  }
  
  object p2 {
    val finalTopCrates: List[Char] = {
      val result = moves
        .foldLeft(stacks)({ (map, move) =>
          val Move(quantity, from, to) = move
          val (moved, retained) = {
            val toSplit = map(from)
            if (toSplit.length > quantity)
              toSplit.splitAt(quantity)
            else
              (toSplit, Nil)
          }
          val updatedMap = map + (from -> retained) + (to -> (moved ::: map(to)))
          updatedMap
        })
        .toList
        .sortBy(_._1)
        .flatMap(_._2.headOption)
    
      result
    }
    
    def answer: String = s"${finalTopCrates.mkString("")}"
  }
}
