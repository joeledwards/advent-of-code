package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day2._

class day2Spec extends UnitSpec {
  "day2.Choice" when {
    "parsing a Choice from the strategy guide" should {
      "convert A to Rock"     in { Choice.of("A") shouldBe Some(Rock) }
      "convert B to Paper"    in { Choice.of("B") shouldBe Some(Paper) }
      "convert C to Scissors" in { Choice.of("C") shouldBe Some(Scissors) }
      "convert X to Rock"     in { Choice.of("X") shouldBe Some(Rock) }
      "convert Y to Paper"    in { Choice.of("Y") shouldBe Some(Paper) }
      "convert Z to Scissors" in { Choice.of("Z") shouldBe Some(Scissors) }
      
      "not recognize any other value" in {
        Choice.of("a") shouldBe None
        Choice.of("b") shouldBe None
        Choice.of("c") shouldBe None
        Choice.of("x") shouldBe None
        Choice.of("y") shouldBe None
        Choice.of("z") shouldBe None
        Choice.of("1") shouldBe None
        Choice.of("D") shouldBe None
        Choice.of("W") shouldBe None
      }
    }
  }
  
  "day2.Outcome" when {
    "parsing an Outcome from the strategy guide" should {
      "convert X to Loss" in { Outcome.of("X") shouldBe Some(Loss) }
      "convert Y to Tie"  in { Outcome.of("Y") shouldBe Some(Tie) }
      "convert Z to Win"  in { Outcome.of("Z") shouldBe Some(Win) }
      
      "not recognize any other value" in {
        Outcome.of("A") shouldBe None
        Outcome.of("B") shouldBe None
        Outcome.of("C") shouldBe None
        Outcome.of("a") shouldBe None
        Outcome.of("b") shouldBe None
        Outcome.of("c") shouldBe None
        Outcome.of("x") shouldBe None
        Outcome.of("y") shouldBe None
        Outcome.of("z") shouldBe None
        Outcome.of("1") shouldBe None
        Outcome.of("D") shouldBe None
        Outcome.of("W") shouldBe None
      }
    }
  }
  
  "day2.Rock" when {
    "competing" should {
      "win against Scissors" in { Rock.vs(Scissors) shouldBe Win }
      "tie against Rock"     in { Rock.vs(Rock) shouldBe Tie }
      "lose against Paper"   in { Rock.vs(Paper) shouldBe Loss }
    }
  }
  
  "day2.Paper" when {
    "competing" should {
      "win against Rock"      in { Paper.vs(Rock) shouldBe Win }
      "tie against Paper"     in { Paper.vs(Paper) shouldBe Tie }
      "lose against Scissors" in { Paper.vs(Scissors) shouldBe Loss }
    }
  }
  
  "day2.Scissors" when {
    "competing" should {
      "win against Paper"    in { Scissors.vs(Paper) shouldBe Win }
      "tie against Scissors" in { Scissors.vs(Scissors) shouldBe Tie }
      "lose against Rock"    in { Scissors.vs(Rock) shouldBe Loss }
    }
  }
  
  "day2.myScore" when {
    "compairing choices" should {
      "award the correct points based on my selection and the outcome" in {
        myScore(Rock, Rock) shouldBe 4L
        myScore(Rock, Paper) shouldBe 1L
        myScore(Rock, Scissors) shouldBe 7L
        myScore(Paper, Rock) shouldBe 8L
        myScore(Paper, Paper) shouldBe 5L
        myScore(Paper, Scissors) shouldBe 2L
        myScore(Scissors, Rock) shouldBe 3L
        myScore(Scissors, Paper) shouldBe 9L
        myScore(Scissors, Scissors) shouldBe 6L
      }
    }
  }
  
  "day2.p1.decode" when {
    "decoding lines" should {
      "translate the strategy guide based on Choices" in {
        p1.decode("A X") shouldBe Some((Rock, Rock))
        p1.decode("A Y") shouldBe Some((Rock, Paper))
        p1.decode("A Z") shouldBe Some((Rock, Scissors))
        p1.decode("B X") shouldBe Some((Paper, Rock))
        p1.decode("B Y") shouldBe Some((Paper, Paper))
        p1.decode("B Z") shouldBe Some((Paper, Scissors))
        p1.decode("C X") shouldBe Some((Scissors, Rock))
        p1.decode("C Y") shouldBe Some((Scissors, Paper))
        p1.decode("C Z") shouldBe Some((Scissors, Scissors))
      }
    }
  }
  
  "day2.p2.decode" when {
    "decoding lines" should {
      "translate the strategy guide based on Outcomes" in {
        p2.decode("A X") shouldBe Some((Rock, Scissors))
        p2.decode("A Y") shouldBe Some((Rock, Rock))
        p2.decode("A Z") shouldBe Some((Rock, Paper))
        p2.decode("B X") shouldBe Some((Paper, Rock))
        p2.decode("B Y") shouldBe Some((Paper, Paper))
        p2.decode("B Z") shouldBe Some((Paper, Scissors))
        p2.decode("C X") shouldBe Some((Scissors, Paper))
        p2.decode("C Y") shouldBe Some((Scissors, Scissors))
        p2.decode("C Z") shouldBe Some((Scissors, Rock))
      }
    }
  }
}
