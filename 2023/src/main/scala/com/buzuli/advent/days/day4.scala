package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day4 extends AdventDay(4) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1CardScoreSums}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2ScoreCardCount}"
  }

  lazy val puzzle1CardScoreSums: Long = {
    cards
      .map(c => {
        c.correctNumbers.foldLeft(0L)({ (acc, _) =>
          acc match {
            case 0 => 1L
            case v: Long => v * 2L
          }
        })
      })
      .sum
  }

  lazy val puzzle2ScoreCardCount: Int = {
    val cardMap = cards.map(c => c.id -> c).toMap

    def collect(card: Card): List[Card] = {
      card :: card
        .correctNumbers
        .toList
        .zipWithIndex
        .flatMap({ case (_, i) =>
          val subId = card.id + i + 1
          cardMap.get(subId) match {
            case Some(c) => collect(c)
            case None => Nil
          }
        })
    }

    cards.flatMap(collect).size
  }

  lazy val cards = lines.flatMap(parseCard(_).toList)

  case class Card(
    id: Int,
    winningSet: Set[Int],
    guessSet: Set[Int],
  ) {
    lazy val correctNumbers: Set[Int] = guessSet & winningSet
  }

  def parseCard(line: String): Option[Card] = line match {
    case s"Card ${id}: ${ws} | ${cs}" => Some(Card(
      id.trim.toInt,
      ws.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet,
      cs.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet,
    ))
    case _ => None
  }
}
