package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle1LowestSeedLocation}"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"${puzzle2SeedRangesLowestLocation}"
  }

  lazy val puzzle1LowestSeedLocation: Long = seeds.map(seedToLocation).min

  lazy val puzzle2SeedRangesLowestLocation: Long = seedsFromRanges.map(seedToLocation).min


  def seedToLocation(seed: Long): Long = {
    var affinity = "seed"
    var value = seed

    while (affinity != "location") {
      srcToMapper.get(affinity) match {
        case Some(mapper) =>
          println(s"${affinity}(${value}) => ${mapper.dstType}(${mapper.map(value)})")
          affinity = mapper.dstType
          value = mapper.map(value)
      }
    }

    println(s"${seed} => ${value}")

    value
  }

  lazy val srcToMapper: Map[String, Mapper] = mappers.map { case ((src, _), mapper) => src -> mapper }

  lazy val seedsFromRanges: List[Long] = {
    val pairs: List[List[Long]] = seeds.grouped(2).toList

    pairs.flatMap({
      case List(start, length) => Range(0, length.toInt).map(start + _)
      case _ => Nil
    })
  }

  lazy val (seeds: List[Long], mappers: Map[(String, String), Mapper]) = {
    var seeds: List[Long] = Nil
    var mappers: Map[(String, String), Mapper] = Map.empty
    var currentMapper: Option[(String, String)] = None

    lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .foreach({
        case s"seeds:${seedStr}" =>
          seeds = seedStr
            .split(" ")
            .view
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(_.toLong)
            .toList
        case s"${from}-to-${to} map:" =>
          currentMapper = Some(from -> to)
        case mappingStr =>
          (
            mappingStr
              .split(" ")
              .view
              .map(_.trim)
              .filter(_.nonEmpty)
              .flatMap(_.toLongOption)
              .toList
          ) match {
            case List(dst, src, length) =>
              currentMapper match {
                case Some((from, to)) =>
                  mappers += from -> to -> {
                      mappers
                        .getOrElse(from -> to, Mapper(from, to, Nil))
                        .plus(RangeMap(src, dst, length))
                  }
                case _ =>
              }
            case _ =>
          }
      })

    seeds -> mappers
  }

  case class Mapper(
    srcType: String,
    dstType: String,
    rangeMaps: List[RangeMap],
  ) {
    def plus(rangeMap: RangeMap): Mapper = this.copy(rangeMaps = rangeMap :: rangeMaps)

    def map(location: Long): Long = {
      rangeMaps
        .collectFirst({ case rm if rm.find(location).nonEmpty => rm })
        .flatMap(_.find(location))
        .getOrElse(location)
    }
  }

  case class RangeMap(
    src: Long,
    dst: Long,
    length: Long,
  ) {
    val limit = src + length

    def find(location: Long): Option[Long] = {
      if (location >= src && location < limit) {
        val offset = location - src
        Some(dst + offset)
      } else {
        None
      }
    }
  }
}
