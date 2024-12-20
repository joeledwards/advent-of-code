package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day6 extends AdventDay(6) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$distinctTilesVisited"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$allLoops"
  }

  case class Tile(
    x: Int,
    y: Int,
    v: Char,
  ) {
    val position: (Int, Int) = x -> y
    val isOccupied: Boolean = v == '#'
    val isStart: Boolean = v == '^'
  }

  val tiles: List[Tile] = {
    val yMax = lines.size - 1
    lines
      .zipWithIndex
      .flatMap({ case (line, yInv) =>
        val y = yMax - yInv
        line
          .toList
          .zipWithIndex
          .map({ case (v, x) =>
            Tile(x, y, v)
          })
      })
  }

  sealed trait Direction {
    def turnRight: Direction = this match {
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up
    }
  }

  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class NavContext(
     tile: Option[Tile],
     direction: Direction,
     visited: Set[Tile]
  )

  def nav(
    tileMap: Map[(Int, Int), Tile]
  )(
    context: NavContext
  ): NavContext = context match {
    case ctx @ NavContext(None, _, _) => ctx
    case NavContext(Some(currentTile), direction, visited) => {
      val Tile(x, y, _) = currentTile
      val candidate = direction match {
        case Up    =>  x      -> (y+1)
        case Down  =>  x      -> (y-1)
        case Left  => (x - 1) ->  y
        case Right => (x + 1) ->  y
      }

      tileMap.get(candidate) match {
        case None => NavContext(None, direction, visited)
        case Some(nextTile) if nextTile.isOccupied =>
          NavContext(Some(currentTile), direction.turnRight, visited)
        case Some(nextTile) => NavContext(Some(nextTile), direction, visited + nextTile)
      }
    }
  }

  def distinctTilesVisited: Long = {
    val tileMap: Map[(Int, Int), Tile] = tiles.map(t => t.position -> t).toMap
    val startTile = tiles.find(_.isStart)
    var context = NavContext(startTile, Up, startTile.toSet)

    def navigate: NavContext => NavContext = nav(tileMap)

    while (context.tile.nonEmpty) {
      context = navigate(context)
    }

    context.visited.size
  }

  def allLoops: Long = {
    val emptyTiles = tiles.filter(t => !(t.isOccupied || t.isStart))
    val baseTileMap: Map[(Int, Int), Tile] = tiles.map(t => t.position -> t).toMap
    val startTile = tiles.find(_.isStart)

    emptyTiles.map({ et =>
      val tileMap = baseTileMap + (et.position -> et.copy(v = '#'))
      val navigate: NavContext => NavContext = nav(tileMap)
      var context = NavContext(startTile, Up, startTile.toSet)
      var navHistory: Set[NavContext] = Set(context)
      var isLoop: Boolean = false
      var steps: Int = 0

      while (context.tile.nonEmpty && !isLoop) {
        context = navigate(context)
        isLoop = navHistory.contains(context)
        navHistory += context
        steps += 1
      }

      println(s"Processing ${et.position} (${steps} steps) ${if (isLoop) "LOOP" else ""}")

      if (isLoop) 1L else 0L
    }).sum
  }
}
