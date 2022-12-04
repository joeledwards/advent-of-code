package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}

object day2 extends AdventDay(2) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    //println(strategyGuide.mkString("\n"))
    s"${p1.guideScores.sum}"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${p2.guideScores.sum}"
  }
  
  sealed abstract class Choice(val value: Long) {
    def vs(other: Choice): Outcome
  }
  case object Rock extends Choice(1) {
    override def vs(other: Choice): Outcome = other match {
      case Scissors => Win
      case Rock     => Tie
      case Paper    => Loss
    }
  }
  case object Paper extends Choice(2) {
    override def vs(other: Choice): Outcome = other match {
      case Rock     => Win
      case Paper    => Tie
      case Scissors => Loss
    }
  }
  case object Scissors extends Choice(3) {
    override def vs(other: Choice): Outcome = other match {
      case Paper    => Win
      case Scissors => Tie
      case Rock     => Loss
    }
  }
  
  object Choice {
    def of(value: String): Option[Choice] = value match {
      case "A" => Some(Rock)
      case "B" => Some(Paper)
      case "C" => Some(Scissors)
      case "X" => Some(Rock)
      case "Y" => Some(Paper)
      case "Z" => Some(Scissors)
      case _ => None
    }
  }
  
  sealed abstract class Outcome(val value: Long) {
    def against(c: Choice): Choice
  }
  case object Win extends Outcome(6) {
    override def against(c: Choice): Choice = c match {
      case Rock => Paper
      case Paper => Scissors
      case Scissors => Rock
    }
  }
  case object Tie extends Outcome(3) {
    override def against(c: Choice): Choice = c match {
      case Rock => Rock
      case Paper => Paper
      case Scissors => Scissors
    }
  }
  case object Loss extends Outcome(0) {
    override def against(c: Choice): Choice = c match {
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper
    }
  }
  
  object Outcome {
    def of(value: String): Option[Outcome] = value match {
      case "X" => Some(Loss)
      case "Y" => Some(Tie)
      case "Z" => Some(Win)
      case _ => None
    }
  }
  
  def myScore(myChoice: Choice, opponentChoice: Choice): Long = myChoice.value + myChoice.vs(opponentChoice).value
  
  object p1 {
    def decode(line: String): Option[(Choice, Choice)] = {
      line match {
        case s"${opponentChoiceS} ${myChoiceS}" =>
          Choice
            .of(opponentChoiceS)
            .flatMap(opponentChoice =>
              Choice
                .of(myChoiceS)
                .map(myChoice => (opponentChoice, myChoice))
            )
        case _ => None
      }
    }
    
    val strategyGuide: List[(Choice, Choice)] = {
      lines
        .flatMap(decode(_).toList)
    }
    
    val guideScores: List[Long] = {
      strategyGuide
        .map({ case (opponentChoice, myChoice) => myScore(myChoice, opponentChoice) })
    }
  }
  
  object p2 {
    def decode(line: String): Option[(Choice, Choice)] = {
      line match {
        case s"${opponentChoiceS} ${outcomeS}" =>
          Choice
            .of(opponentChoiceS)
            .flatMap(opponentChoice =>
              Outcome
                .of(outcomeS)
                .map(outcome => (opponentChoice, outcome.against(opponentChoice)))
            )
        case _ => None
      }
    }
    
    val strategyGuide: List[(Choice, Choice)] = {
      lines
        .flatMap(decode(_).toList)
    }
    
    val guideScores: List[Long] = {
      strategyGuide
        .map({ case (opponentChoice, myChoice) => myScore(myChoice, opponentChoice) })
    }
  }
}
