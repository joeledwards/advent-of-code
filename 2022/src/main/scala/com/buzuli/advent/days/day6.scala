package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import java.util
import scala.concurrent.{ExecutionContext, Future}

object day6 extends AdventDay(6) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  class CountingSet[T] {
    private var map: Map[T, Int] = Map.empty[T, Int]
    private var _historicalCount: Int = 0
    private var _count: Int = 0
    private var history: util.Deque[T] = new util.LinkedList
    
    def add(e: T): Int = {
      _count += 1
      _historicalCount += 1
      history.addFirst(e)
      
      val count = map.getOrElse(e, 0) + 1
      map += (e -> count)
      count
    }
  
    /**
     * Attempt to remove an element and indicate the outcome.
     *
     * @return None if no element was found. Otherwise Some(remainingCount). Some(0) means it was just removed.
     */
    private def removeElement(e: T): Option[Int] = {
      map.get(e) match {
        case None => None
        case Some(1) => {
          map -= e
          Some(0)
        }
        case Some(v) => {
          map += (e -> (v - 1))
          Some(v - 1)
        }
      }
    }
  
    def remove(e: T): Option[Int] = {
      removeElement(e) match {
        case None => None
        case remaining => {
          _count -= 1
          history.removeLastOccurrence(e)
          remaining
        }
      }
    }
    
    def pop(): Option[(T, Int)] = {
      history.isEmpty match {
        case true => None
        case false => {
          _count -= 1
          val e = history.removeLast()
          removeElement(e).map(r => (e, r))
        }
      }
    }
  
    def historicalCount: Int = _historicalCount
    def count: Int = _count
    def size: Int = map.size
  }
  
  object p1 {
    def decode(v: String): Int = {
      val history: CountingSet[Char] = new CountingSet
  
      v.view.takeWhile({ c =>
        if (history.count == 4) {
          history.pop()
        }
        history.add(c)
        //println(s"${c} ${history.size}")
        history.size != 4
      }).toList
  
      history.historicalCount
    }
    
    val skipCount = lines match {
      case head :: _ => decode(head)
      case _ => 0
    }
    
    def answer: String = s"${skipCount}"
  }
  
  object p2 {
    def decode(v: String): Int = {
      val history: CountingSet[Char] = new CountingSet
    
      v.view.takeWhile({ c =>
        if (history.count == 14) {
          history.pop()
        }
        history.add(c)
        //println(s"${c} ${history.size}")
        history.size != 14
      }).toList
    
      history.historicalCount
    }
  
    val skipCount = lines match {
      case head :: _ => decode(head)
      case _ => 0
    }
  
    def answer: String = s"${skipCount}"
  }
}
