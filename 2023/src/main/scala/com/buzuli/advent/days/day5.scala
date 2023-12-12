package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day5 extends AdventDay(5) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    //s"${puzzle1LowestSeedLocation}"
    ""
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
          //println(s"${affinity}(${value}) => ${mapper.dstType}(${mapper.map(value)})")
          affinity = mapper.dstType
          value = mapper.map(value)
      }
    }

    //println(s"${seed} => ${value}")

    value
  }

  lazy val srcToMapper: Map[String, RangeMapper] = mappers.map { case ((src, _), mapper) => src -> mapper }

  lazy val seedsFromRanges: List[Long] = {
    val pairs: List[List[Long]] = seeds.grouped(2).toList

    pairs.flatMap({
      case List(start, length) => Range(0, length.toInt).map(start + _)
      case _ => Nil
    })
  }

  lazy val (seeds: List[Long], mappers: Map[(String, String), RangeMapper]) = {
    var seeds: List[Long] = Nil
    var mappers: Map[(String, String), RangeMapper] = Map.empty
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
                        .getOrElse(from -> to, RangeMapper(from, to, Nil))
                        .plus(RangeMap(src, dst, length))
                  }
                case _ =>
              }
            case _ =>
          }
      })

    seeds -> mappers
  }

  case class RangeMapper(
    srcType: String,
    dstType: String,
    rangeMaps: List[RangeMap],
  ) {
    def plus(rangeMap: RangeMap): RangeMapper = this.copy(rangeMaps = rangeMap :: rangeMaps)

    def map(location: Long): Long = {
      rangeMaps
        .collectFirst({ case rm if rm.find(location).nonEmpty => rm })
        .flatMap(_.find(location))
        .getOrElse(location)
    }

    /**
     * Merge two RangeMappers such that the input of this maps directly to the output of other.
     * The effect is that running newMapping.map(N) is the same as running this.map(N).flatMap(other.map)
     *
     * @param other the RangeMapper with with to merge this RangeMapper
     *
     * @return the new, composite RangeMapper
     */
    def merge(other: RangeMapper): RangeMapper = {
      // TODO: create a new RangeMapper with a new List of RangeMap which map the
      //       inputs of the RangeMaps from this to the output of the RangeMaps from other

      // 1) Order RangeMaps by their contents
      // 2) Iterate over the pair of lists, identifying all of the overlaps
      // 3) Merge all of the overlaps; split out all of the non-overlapping

      val cutPoints: List[Long] = List(
        this.rangeMaps.flatMap(r => List(r.dstMin, r.dstMax)),
        other.rangeMaps.flatMap(r => List(r.srcMin, r.srcMax))
      ).flatten.toSet.toList.sorted

      val srcs = this.rangeMaps.flatMap({ rm =>
        cutPoints.flatMap({ point =>
          if (point >= rm.srcMin && point <= rm.srcMax) {
            Nil
            //rm.split(point)
          } else {
            Nil
          }
        })
      })
      val dsts = other.rangeMaps.sortBy(_.src)

      val compositeRangeMaps: List[RangeMap] = Nil

      RangeMapper(
        this.srcType,
        other.dstType,
        compositeRangeMaps
      )
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

    val srcMin = src
    val srcMax = src + length - 1
    val dstMin = dst
    val dstMax = dst + length - 1
  }
}