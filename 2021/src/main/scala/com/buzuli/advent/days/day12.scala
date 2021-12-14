package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day12 extends AdventDay(12) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    "--"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    "--"
  }

  val caves: mutable.Map[String, CaveNode] = mutable.Map.empty
  val caveSystem: CaveStart = {
    val start = CaveStart()

    caves.put("start", start)
    caves.put("end", CaveEnd())

    sample
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map {
        case s"${from}-${to}" => {
          val fromNode = caves.getOrElseUpdate(from, CaveNode.forName(from))
          val toNode = caves.getOrElseUpdate(to, CaveNode.forName(to))
          fromNode.links.add(toNode)
          toNode.links.add(fromNode)
        }
      }

    start
  }

  lazy val sample: List[String] = {
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |""".stripMargin.split("\n").toList
  }
}

sealed trait CaveNode {
  val links: mutable.Set[CaveNode] = mutable.Set.empty
}
case class CaveStart() extends CaveNode
case class CaveSmall(name: String) extends CaveNode
case class CaveLarge(name: String) extends CaveNode
case class CaveEnd() extends CaveNode

object CaveNode {
  def forName(name: String): CaveNode = name match {
    case "start" => CaveStart()
    case "end" => CaveEnd()
    case n if n.toLowerCase == n => CaveSmall(n)
    case n => CaveLarge(n)
  }
}