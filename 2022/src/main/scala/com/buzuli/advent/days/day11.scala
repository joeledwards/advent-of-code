package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day11 extends AdventDay(11) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  case class Monkey(
    id: Long,
    factor: Long = 0,
    idT: Long = 0,
    idF: Long = 0,
    startItems: List[Long] = Nil,
    operation: Option[Long => Long] = None,
    worryControl: Option[Long => Long] = None
  ) {
    private var items: List[Long] = startItems
    private var inspectionCount: Long = 0
    
    def inspections: Long = inspectionCount
    def receive(item: Long): Unit = items = item :: items
    
    def inspectItems(worryReduction: Boolean): List[(Long, Long)] = {
      val readyToThrow = items.flatMap { worry =>
        inspectionCount += 1
        operation
          .map(_(worry))
          .flatMap(w => worryControl.map(_(w)))
          .map(w => if (w % factor == 0) (w, idT) else (w, idF))
          .toList
      }
      items = Nil
      readyToThrow
    }
  
    override def toString: String = s"Monkey ${id}: ${items.mkString(", ")} (${inspections} inspections)"
  }
  
  def getMonkeys: List[Monkey] = lines.foldLeft[List[Monkey]](Nil)({ (acc, line) =>
    (acc, line) match {
      case (l, s"Monkey ${id}:") => Monkey(id.toLong) :: l
      case (m :: r, s"  Starting items: ${itemStr}") => m.copy(startItems = itemStr.split(",").toList.map(_.trim).map(_.toLong)) :: r
      case (m :: r, s"  Operation: new = old * old") => m.copy(operation = Some((i: Long) => i * i)) :: r
      case (m :: r, s"  Operation: new = old + old") => m.copy(operation = Some((i: Long) => i + i)) :: r
      case (m :: r, s"  Operation: new = old * ${value}") => m.copy(operation = Some(value.toLong).map(v => (i: Long) => v * i)) :: r
      case (m :: r, s"  Operation: new = old + ${value}") => m.copy(operation = Some(value.toLong).map(v => (i: Long) => v + i)) :: r
      case (m :: r, s"  Test: divisible by ${value}") => m.copy(factor = value.toLong) :: r
      case (m :: r, s"    If true: throw to monkey ${id}") => m.copy(idT = id.toLong) :: r
      case (m :: r, s"    If false: throw to monkey ${id}") => m.copy(idF = id.toLong) :: r
      case _ => acc
    }
  }).reverse
  
  object p1 {
    val monkeys: List[Monkey] = getMonkeys.map(_.copy(worryControl = Some((w: Long) => w/3)))
    val monkeyMap: Map[Long, Monkey] = monkeys.map(m => (m.id, m)).toMap
    
    for (round <- Range.inclusive(1, 20)) {
      monkeys.foreach { monkey =>
        val toToss = monkey.inspectItems(true)
        toToss.foreach { case (item, target) =>
          monkeyMap.get(target).map(_.receive(item))
        }
      }
      //println(s"Round ${round}:\n${monkeys.mkString("\n")}\n")
    }
    
    val totalImpact: Long = monkeys.map(_.inspections).sorted.reverse.take(2).reduce(_*_)
    
    def answer: String = s"${totalImpact}"
  }
  
  object p2 {
    val baseMonkeys: List[Monkey] = getMonkeys
    val modulo: Long = baseMonkeys.map(_.factor).foldLeft(1L)(_*_)
    val monkeys: List[Monkey] = baseMonkeys.map(_.copy(worryControl = Some((w: Long) => w % modulo)))
    val monkeyMap: Map[Long, Monkey] = monkeys.map(m => (m.id, m)).toMap
    
    for (round <- Range.inclusive(1, 10000)) {
      monkeys.foreach { monkey =>
        val toToss = monkey.inspectItems(false)
        toToss.foreach { case (item, target) =>
          monkeyMap.get(target).map(_.receive(item))
        }
      }
      //println(s"Round ${round}:\n${monkeys.mkString("\n")}\n")
      println(s"Round: ${round}")
    }
  
    monkeys.foreach { monkey =>
      println(s"Monkey ${monkey.id} inspections => ${monkey.inspections}")
    }
    val totalImpact: Long = monkeys.map(_.inspections).sorted.reverse.take(2).reduce(_*_)
  
    def answer: String = s"${totalImpact}"
  }
}