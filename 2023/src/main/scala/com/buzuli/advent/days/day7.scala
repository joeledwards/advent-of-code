package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day7 extends AdventDay(7) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1HandValues}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2HandValuesWithWilds}"
  }

  lazy val puzzle1HandValues: Long = {
    lines
      .flatMap({
        case s"${handStr} ${bidStr}" => Some((Hand(handStr.toList.flatMap(Card.fromChar(_))), Bid(bidStr.toLong)))
        case _ => None
      })
      .sortBy({ case (hand, _) => hand.ordering })
      .zipWithIndex
      .map({ case ((hand, bid), i) =>
        println(s"${i + 1} ${hand} [${hand.ordering}]")
        ((hand, bid), i)
      })
      .map({ case ((_, bid), index) => bid.amount * (index + 1)})
      .sum
  }

  lazy val puzzle2HandValuesWithWilds: Long = {
    lines
      .flatMap({
        case s"${handStr} ${bidStr}" => Some((
          Hand(
            handStr
              .toList
              .flatMap(Card.fromChar(_, true))
          ),
          Bid(bidStr.toLong)
        ))
        case _ => None
      })
      .sortBy({ case (hand, _) => hand.ordering })
      .zipWithIndex
      .map({ case ((hand, bid), i) =>
        println(s"${i + 1} ${hand} [${hand.ordering}]")
        ((hand, bid), i)
      }
      )
      .map({ case ((_, bid), index) => bid.amount * (index + 1) })
      .sum
  }

  Hand

  case class Bid(amount: Long)

  sealed abstract class Card(val rank: Long)

  case object Ace extends Card(rank = 14)
  case object King extends Card(rank = 13)
  case object Queen extends Card(rank = 12)
  case object Jack extends Card(rank = 11)
  case object Ten extends Card(rank = 10)
  case object Nine extends Card(rank = 9)
  case object Eight extends Card(rank = 8)
  case object Seven extends Card(rank = 7)
  case object Six extends Card(rank = 6)
  case object Five extends Card(rank = 5)
  case object Four extends Card(rank = 4)
  case object Three extends Card(rank = 3)
  case object Two extends Card(rank = 2)
  case object Joker extends Card(rank = 1)

  case class Hand(cards: List[Card]) {
    lazy val strength: HandStrength = {
      (
        cards
          .groupBy(_.rank)
          .toList
          .map({ case (_, cards) => (cards.head, cards.length) })
          .sortBy({
            case (Joker, length) => (1, length)
            case (_, length) => (0, length)
          })
          .reverse
      ) match {
        case (Joker, 4) :: _ => FiveOfAKind
        case (Joker, 3) :: (_, 2) ::  _ => FiveOfAKind
        case (Joker, 3) :: (_, 1) ::  _ => FourOfAKind
        case (Joker, 2) :: (_, 3) ::  _ => FiveOfAKind
        case (Joker, 2) :: (_, 2) ::  _ => FourOfAKind
        case (Joker, 2) :: (_, 1) ::  _ => ThreeOfAKind
        case (Joker, 1) :: (_, 4) ::  _ => FiveOfAKind
        case (Joker, 1) :: (_, 3) ::  _ => FourOfAKind
        case (Joker, 1) :: (_, 2) :: (_, 2) :: _ => FullHouse
        case (Joker, 1) :: (_, 2) :: _ => ThreeOfAKind
        case (Joker, 1) :: _ => OnePair
        case (_, 5) :: _ => FiveOfAKind
        case (_, 4) :: _ => FourOfAKind
        case (_, 3) :: (_, 2) :: _ => FullHouse
        case (_, 3) :: _ => ThreeOfAKind
        case (_, 2) :: (_, 2) :: _=> TwoPair
        case (_, 2) :: _=> OnePair
        case _ => HighCard
      }
    }

    lazy val ordering: (Long, Long, Long, Long, Long, Long) = cards.map(_.rank) match {
      case List(a, b, c, d, e) => (strength.value, a, b, c, d, e)
      case _ => (strength.value, 0, 0, 0, 0, 0)
    }
  }

  sealed abstract class HandStrength(val value: Long)

  case object HighCard extends HandStrength(value = 0)
  case object OnePair extends HandStrength(value = 1)
  case object TwoPair extends HandStrength(value = 2)
  case object ThreeOfAKind extends HandStrength(value = 3)
  case object FullHouse extends HandStrength(value = 4)
  case object FourOfAKind extends HandStrength(value = 5)
  case object FiveOfAKind extends HandStrength(value = 6)

  object Card {
    def fromChar(c: Char, withJokers: Boolean = false): Option[Card] = {
      c match {
        case 'A' => Some(Ace)
        case 'K' => Some(King)
        case 'Q' => Some(Queen)
        case 'J' if withJokers => Some(Joker)
        case 'J' => Some(Jack)
        case 'T' => Some(Ten)
        case '9' => Some(Nine)
        case '8' => Some(Eight)
        case '7' => Some(Seven)
        case '6' => Some(Six)
        case '5' => Some(Five)
        case '4' => Some(Four)
        case '3' => Some(Three)
        case '2' => Some(Two)
        case _ => None
      }
    }
  }
}