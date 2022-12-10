package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import java.util
import scala.concurrent.{ExecutionContext, Future}

object day10 extends AdventDay(10) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  sealed trait Instruction
  case object Noop extends Instruction
  case class Addx(value: Long) extends Instruction
  
  val instructions = lines.flatMap {
    case "noop" => List(Noop)
    case s"addx ${value}" => value.toLongOption.map(Addx).toList
  }
  
  object p1 {
    var register: Long = 1L
    var cycle: Long = 0L
    var nextEval: Int = 20
    var signalStrength: Long = 0L
    
    def step(): Unit = {
      cycle += 1
      nextEval -= 1
      if (nextEval == 0) {
        signalStrength += (cycle * register)
        nextEval = 40
      }
    }
    
    instructions.foreach {
      case Noop => {
        step()
      }
      case Addx(value) => {
        step()
        step()
        register += value
      }
    }
    
    def answer: String = s"${signalStrength}"
  }
  
  object p2 {
    val screen: Array[Array[Char]] = Array.fill(6, 40)(' ')
    var register: Long = 1L
    var x: Int = 0
    var y: Int = 0
  
    def step(): Unit = {
      screen(y)(x) = (register - x) match {
        case -1 | 0 | 1 => '#'
        case _ => ' '
      }
      
      if (x == 39) {
        x = 0
        y += 1
      } else {
        x += 1
      }
    }
  
    instructions.foreach {
      case Noop => {
        step()
      }
      case Addx(value) => {
        step()
        step()
        register += value
      }
    }
    
    val output = screen.toList.map(_.toList.mkString("")).mkString("\n")
    println(output)
    
    def answer: String = s"See stdout"
  }
}