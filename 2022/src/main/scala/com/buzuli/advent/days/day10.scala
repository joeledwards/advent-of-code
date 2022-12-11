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
  
  class CPU(onTick: () => Unit) {
    var register: Long = 1L
    
    def exec(instruction: Instruction): Unit = instruction match {
      case Noop => onTick()
      case Addx(value) => {
        onTick()
        onTick()
        register += value
      }
    }
  }
  
  val instructions = lines.flatMap {
    case "noop" => List(Noop)
    case s"addx ${value}" => value.toLongOption.map(Addx).toList
  }
  
  object p1 {
    var register: Long = 1L
    var cycle: Long = 0L
    var nextEval: Int = 20
    var signalStrength: Long = 0L
    
    def onTick(): Unit = {
      cycle += 1
      nextEval -= 1
      if (nextEval == 0) {
        signalStrength += (cycle * register)
        nextEval = 40
      }
    }
    
    val cpu: CPU = new CPU(onTick)
    instructions.foreach(cpu.exec)
    
    def answer: String = s"${signalStrength}"
  }
  
  case class Coord(var x: Int, var y: Int)
  
  object p2 {
    val screen: Array[Array[Char]] = Array.fill(6, 40)(' ')
    var register: Long = 1L
    var c: Coord = Coord(0, 0)
  
    def onTick(): Unit = {
      screen(c.y)(c.x) = (register - c.x) match {
        case -1 | 0 | 1 => '#'
        case _ => ' '
      }
  
      c = c.x match {
        case 39 => Coord(0, c.y + 1)
        case _  => Coord(c.x + 1, c.y)
      }
    }
    
    val cpu: CPU = new CPU(onTick)
    instructions.foreach(cpu.exec)
    
    val output = screen.toList.map(_.toList.mkString("")).mkString("\n")
    println(output)
    
    def answer: String = s"See stdout"
  }
}