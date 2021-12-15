package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day14 extends AdventDay(14) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (min, max) = minMax(expandTemplate(template)(10))
    (max - min).toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val (min, max) = minMax(expandTemplate(template)(40))
    (max - min).toString
  }

  def minMax(pairMap: Map[(Char, Char), Long]): (Long, Long) = {
    val charMap = mutable.Map[Char, Long]()

    pairMap foreach {
      case ((a, b), count) => {
        charMap.updateWith(a)(_.orElse(Some(0L)).map(_+count))
        charMap.updateWith(b)(_.orElse(Some(0L)).map(_+count))
      }
    }

    charMap.keys.toList.foreach { key =>
      charMap.updateWith(key)(_.map(_ / 2))
    }

    charMap.updateWith(template.head)(_.orElse(Some(0L)).map(_+1L))
    charMap.updateWith(template.last)(_.orElse(Some(0L)).map(_+1L))

    val counts = charMap.toList.map({ case (k, v) => (v, k) }).sorted
    val min = counts.head._1
    val max = counts.last._1

    (min, max)
  }

  def expandTemplate(template: List[Char])(iterations: Int): Map[(Char, Char), Long] = {
    var pairCountsMap = {
      val pairs = template.reverse.tail.reverse.zip(template.tail)
      val map = mutable.Map[(Char, Char), Long]()
      pairs.foreach {
        case (a, b) => map.updateWith((a, b))(_.orElse(Some(0L)).map(_+1L))
      }
      map
    }

    for (_ <- 1 to iterations) {
      val newMap = mutable.Map[(Char, Char), Long]()

      pairCountsMap foreach {
        case ((a, b), count) => {
          rules.get((a, b)) foreach { c =>
            newMap.updateWith((a, c))(_.orElse(Some(0L)).map(_+count))
            newMap.updateWith((c, b))(_.orElse(Some(0L)).map(_+count))
          }
        }
      }

      pairCountsMap = newMap
    }

    pairCountsMap.toMap
  }

  lazy val template: List[Char] = lines.head.toList

  lazy val rules: Map[(Char, Char), Char] = {
    lines
      .tail
      .map(_.trim)
      .filter(_.nonEmpty)
      .map {
        case s"${pair} -> ${insertion}" => pair.toList match {
          case List(a, b) => (a, b)-> insertion.head
        }
      } toMap
  }

  lazy val sample: List[String] = {
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C
      |""".stripMargin.split("\n").toList
  }
}