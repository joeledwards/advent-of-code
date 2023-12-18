package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day15 extends AdventDay(15) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1HashSum}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2Lensing}"
  }

  lazy val puzzle1HashSum: Long = {
    lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(_.split(',').toList)
      .map(hash)
      .sum
  }

  lazy val puzzle2Lensing: Long = {
    var nextSeq: Long = 0
    var lensMap: Map[Long, Map[String, Slide]] = {
      Range(0, 256).map(_.toLong).toList.map((_ -> Map.empty[String, Slide])).toMap
    }

    lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(_.split(',').toList)
      .map[LensUpdate]({
        case s"${label}=${focalLength}" => AddLens(Lens(label, focalLength.toLong))
        case s"${label}-" => RemoveLens(label)
      })
      .foreach({
        case AddLens(lens) => {
          val boxMap = lensMap.getOrElse(lens.labelHash, Map.empty)
          val slide: Slide = boxMap.get(lens.label) match {
            case None => {
              // If this is a new label, advance the sequence number
              nextSeq += 1
              Slide(lens, nextSeq)
            }
            case Some(Slide(_, oldSeq)) => {
              // If we are replacing the label, retain the sequence number
              Slide(lens, oldSeq)
            }
          }
          // Add or replace the slide
          lensMap = lensMap + (lens.labelHash -> (boxMap + (lens.label -> slide)))
        }
        case r: RemoveLens => {
          lensMap.get(r.labelHash) match {
            case None =>
            case Some(boxMap) => {
              // Remove the slide if it exists
              lensMap = lensMap + (r.labelHash -> (boxMap - r.label))
            }
          }
        }
      })

      lensMap
        .toList
        .sortBy(_._1)
        .zipWithIndex
        .map({ case ((_, boxMap), boxIndex) =>
          boxMap
            .toList
            .sortBy(_._2.seq)
            .zipWithIndex
            .map({ case ((_, Slide(Lens(_, focalLength), _)), lensIndex) =>
              val boxPosition = boxIndex + 1
              val lensPosition = lensIndex + 1
              boxPosition * lensPosition * focalLength
            })
            .sum
        })
        .sum
  }

  case class Slide(
    lens: Lens,
    seq: Long,
  )

  case class Lens(label: String, focalLength: Long) {
    val labelHash: Long = hash(label)
  }

  sealed trait LensUpdate

  case class AddLens(lens: Lens) extends LensUpdate

  case class RemoveLens(label: String) extends LensUpdate {
    val labelHash: Long = hash(label)
  }

  def hash(input: String): Long = {
    input
      .foldLeft(0L)({ (h, c) =>
        var d = h + c.toLong
        d = d * 17
        d = d % 256
        d
      }
      )
  }
}