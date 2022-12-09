package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day9 extends AdventDay(9) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  sealed trait Direction {
    def from(coordinate: (Int, Int)): (Int, Int) = {
      (this, coordinate) match {
        case (Up, (x, y)) => (x, y - 1)
        case (Down, (x, y)) => (x, y + 1)
        case (Left, (x, y)) => (x - 1, y)
        case (Right, (x, y)) => (x + 1, y)
      }
    }
  }
  
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
  
  case class Move(direction: Direction, quantity: Int)
  
  val moves: List[Move] = lines.flatMap {
    case s"U ${q}" => q.toIntOption.map(Move(Up, _))
    case s"D ${q}" => q.toIntOption.map(Move(Down, _))
    case s"L ${q}" => q.toIntOption.map(Move(Left, _))
    case s"R ${q}" => q.toIntOption.map(Move(Right, _))
    case _ => None
  }
  
  def follow(leader: (Int, Int), follower: (Int, Int)): (Int, Int) = {
    val (hx, hy) = leader
    val (tx, ty) = follower
  
    //println(s"leader=${leader} follower=${follower} diff=(${hx - tx}, ${hy - ty})")
  
    (hx - tx, hy - ty) match {
      // Overlapping
      case ( 0,  0) => follower
      // Vertically adjacent
      case ( 0, -1) => follower
      case ( 0,  1) => follower
      // Horizontally adjacent
      case (-1,  0) => follower
      case ( 1,  0) => follower
      // Diagonally adjacent
      case (-1, -1) => follower
      case (-1,  1) => follower
      case ( 1, -1) => follower
      case ( 1,  1) => follower
      // Vertically removed
      case ( 0, -2) => (tx, ty-1)
      case ( 0,  2) => (tx, ty+1)
      // Horizontally removed
      case (-2,  0) => (tx-1, ty)
      case ( 2,  0) => (tx+1, ty)
      // Diagonally removed
      case ( 1,  2) => (tx+1, ty+1)
      case ( 2,  1) => (tx+1, ty+1)
      case ( 1, -2) => (tx+1, ty-1)
      case ( 2, -1) => (tx+1, ty-1)
      case (-1,  2) => (tx-1, ty+1)
      case (-2,  1) => (tx-1, ty+1)
      case (-1, -2) => (tx-1, ty-1)
      case (-2, -1) => (tx-1, ty-1)
      // Only possible with part 2
      case ( 2,  2) => (tx+1, ty+1)
      case ( 2, -2) => (tx+1, ty-1)
      case (-2,  2) => (tx-1, ty+1)
      case (-2, -2) => (tx-1, ty-1)
    }
  }
  
  object p1 {
    var head: (Int, Int) = (0, 0)
    var tail: (Int, Int) = head
    var visited: Set[(Int, Int)] = Set(tail)
    
    moves.foreach({ move =>
      for (_ <- Range(0, move.quantity)) {
        head = move.direction.from(head)
        tail = follow(head, tail)
        visited += tail
      }
    })
    
    def answer: String = s"${visited.size}"
  }
  
  object p2 {
    var rope: List[(Int, Int)] = Range(0, 10).toList.map(_ => (0, 0))
    var visited: Set[(Int, Int)] = Set((0, 0))
  
    // Process every move
    moves.foreach({ move =>
      // Process each step for each move
      for (_ <- Range(0, move.quantity)) {
        var head :: rest = rope
        
        // Thead head of the rope is updated as before
        head = move.direction.from(head)
        
        // Every segment must follow that which leads it,
        // so we update them all here
        var leader = head
        rope = head :: (
          rest map { follower =>
            leader = follow(leader, follower)
            leader
          }
        )
        
        // Update coordinates visited by the tail of the rope
        visited += leader
      }
    })
    
    def answer: String = s"${visited.size}"
  }
}