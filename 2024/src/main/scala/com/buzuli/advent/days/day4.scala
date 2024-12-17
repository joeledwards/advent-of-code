package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

object day4 extends AdventDay(4) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$xmasCount"
  }
  
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$xMasCount"
  }

  case class Letter(x: Long, y: Long, value: String) {
    def n: Letter  = copy(y=y+1)
    def s: Letter  = copy(y=y-1)
    def e: Letter  = copy(x=x+1)
    def w: Letter  = copy(x=x-1)
    def nw: Letter = copy(x=x-1, y=y+1)
    def ne: Letter = copy(x=x+1, y=y+1)
    def se: Letter = copy(x=x+1, y=y-1)
    def sw: Letter = copy(x=x-1, y=y-1)

    def masXCandidates(xMax: Long, yMax: Long): List[(Letter, Letter, Letter, Letter)] = {
      if (value == "A") {
        val candidates = List((nw, ne, se, sw))
        candidates
      } else {
        Nil
      }
    }

    def xmasVectorCandidates(xMax: Long, yMax: Long): List[(Letter, Letter, Letter)] = {
      if (value == "X") {
        val vector = {
          (if (y+3 > yMax) Nil else List((n, n.n, n.n.n))) :::
            (if (y-3 < 0)    Nil else List((s, s.s, s.s.s))) :::
            (if (x+3 > xMax) Nil else List((e, e.e, e.e.e))) :::
            (if (x-3 < 0)    Nil else List((w, w.w, w.w.w))) :::
            (if (y+3 > yMax || x-3 < 0   ) Nil else List((nw, nw.nw, nw.nw.nw))) :::
            (if (y+3 > yMax || x+3 > xMax) Nil else List((ne, ne.ne, ne.ne.ne))) :::
            (if (y-3 < 0    || x-3 < 0   ) Nil else List((sw, sw.sw, sw.sw.sw))) :::
            (if (y-3 < 0    || x+3 > xMax) Nil else List((se, se.se, se.se.se))) :::
            Nil
        }
        vector
      } else {
        Nil
      }
    }

    def key: (Long, Long) = x -> y
  }

  lazy val letters = lines
    .reverse
    .zipWithIndex
    .flatMap(yPair => {
      val (line, y) = yPair
      line
        .split("")
        .zipWithIndex
        .map(xPair => {
          val (letter, x) = xPair
          Letter(x, y, letter)
        })
    })

  lazy val map = letters
    .map(letter => letter.key -> letter)
    .toMap

  lazy val xMax: Long = letters.map(_.x).max
  lazy val yMax: Long = letters.map(_.y).max

  def xmasCount: Long = {
    def checkVector(vector: (Letter, Letter, Letter)): Boolean = {
      val (m, a, s) = vector
      map.get(m.key).exists(_.value == "M") &&
      map.get(a.key).exists(_.value == "A") &&
      map.get(s.key).exists(_.value == "S")
    }

    letters
      .flatMap(l => l.xmasVectorCandidates(xMax, yMax))
      .count(checkVector)
  }

  def xMasCount: Long = {
    def checkX(x: (Letter, Letter, Letter, Letter)): Boolean = {
      val (a, b, c, d) = x
      val letters = List(a, b, c, d).flatMap(l => map.get(l.key))
      val ms = letters.filter(_.value == "M")
      val ss = letters.filter(_.value == "S")

      if (ms.size != 2)
        false
      else if (ss.size != 2)
        false
      else
        ms match {
          case List(a, b) if a.x == b.x || a.y == b.y => true
          case _ => false
        }
    }

    letters
      .flatMap(l => l.masXCandidates(xMax, yMax))
      .count(checkX)
  }
}
