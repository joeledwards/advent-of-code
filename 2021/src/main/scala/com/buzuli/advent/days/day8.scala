package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day8 extends AdventDay(8) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    messages
      .foldLeft(0) { (acc, message) =>
        acc + message.outputs.count {
          _.size match {
            case 2 => true
            case 3 => true
            case 4 => true
            case 7 => true
            case _ => false
          }
        }
      }
      .toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val total = messages.map(_.decodedValue).sum.toString
    total
  }

  case class Message(
    messageText: String
  ) {
    val (inputs: List[Set[Char]], outputs: List[Set[Char]]) = {
      messageText.split('|').toList match {
        case in :: out :: Nil => (
          in.split(' ').toList.filter(_.nonEmpty).sortBy(_.length).map(_.toSet),
          out.split(' ').toList.filter(_.nonEmpty).map(_.toSet)
        )
        case _ => throw new Exception(s"Invalid message: ${messageText}")
      }
    }

    lazy val decodeMap: Map[Set[Char], Char] = {
      var List(one, seven, four, f1, f2, f3, s1, s2, s3, eight) = inputs

      val middles = f1 & f2 & f3

      val three = one | middles
      val nine = four | middles

      val left = eight -- three
      val ll = eight -- nine
      val ul = left -- ll

      val middle = four -- one -- ul

      val zero = eight -- middle
      val six = (Set(s1, s2, s3) - zero - nine).head

      val lr = six -- middles -- left

      val five = middles | ul | lr
      val two = (Set(f1, f2, f3) - three - five).head

      Map(
        zero -> '0',
        //  one -> '1',
        two -> '2',
        three -> '3',
        //  four -> '4',
        five -> '5',
        six -> '6',
        //  seven -> '7',
        //  eight -> '8',
        nine -> '9',
      )
    }

    lazy val decodedValue: Int = {
      val decoded = outputs.map({ o =>
         o.size match {
          case 2 => '1'
          case 3 => '7'
          case 4 => '4'
          case 7 => '8'
          case _ => decodeMap(o)
        }
      }).mkString

      decoded.toInt
    }
  }

  val messages: List[Message] = {
    lines
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Message)
      .toList
  }
}
