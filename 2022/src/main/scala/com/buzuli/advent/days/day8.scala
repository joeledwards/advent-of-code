package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.util.Time

import scala.concurrent.{ExecutionContext, Future}

object day8 extends AdventDay(8) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  def transposeGrid[T](grid: List[List[T]]): List[List[T]] = {
    val width = grid(0).size
    val height = grid.size
    Range(0, width).toList map { j =>
      Range(0, height).toList map { i =>
        grid(i)(j)
      }
    }
  }
  
  case class Tree(x: Int, y: Int, height: Int)
  
  type Grid = List[List[Tree]]
  
  val grid: Grid = lines.zipWithIndex.map { case (r, y) =>
    r.toCharArray.toList.zipWithIndex.map { case (h, x) =>
      Tree(x, y, h.toInt)
    }
  }
  
  object p1 {
    def visibleTrees(grid: Grid): Set[Tree] = {
      grid.foldLeft(Set.empty[Tree])({ (visible, row) =>
        row.foldLeft((Set.empty[Tree], 0))({ (acc, tree) =>
          val (visible, maxHeight) = acc
          if (tree.height > maxHeight) {
            (visible + tree, tree.height)
          } else {
            acc
          }
        })._1 | visible
      })
    }
  
    val north: Grid = transposeGrid(grid)
    val south: Grid = north.map(_.reverse)
    val east: Grid = grid
    val west: Grid = east.map(_.reverse)
  
    val visible: Set[Tree] = {
      visibleTrees(north) | visibleTrees(south) | visibleTrees(east) | visibleTrees(west)
    }
    
    def answer: String = s"${visible.size}"
  }
  
  object p2 {
    case class Forest(trees: List[List[Int]]) {
      val height = trees.size
      val width = trees.head.size
      val size = height * width
      val heightMap: Array[Array[Int]] = trees.map(_.toArray).toArray
      
      def heightAt(x: Int, y: Int): Int = heightMap(y)(x)
    }
    
    sealed trait Direction
    case object North extends Direction
    case object South extends Direction
    case object East extends Direction
    case object West extends Direction
    
    def viewFrom(tree: Tree, direction: Direction, forest: Forest): Option[Int] = {
      val height = forest.height
      val width = forest.width
      ((direction, tree.x, tree.y) match {
        case (North, _, 0) => Left(0)
        case (North, _, 1) => Left(1)
        case (North, _, _) => Right(Range.inclusive(tree.y - 1, 0, -1).toList.map((tree.x, _)))
        
        case (South, _, y) if y == height - 1 => Left(0)
        case (South, _, y) if y == height - 2 => Left(1)
        case (South, _, _) => Right(Range(tree.y + 1, height).toList.map((tree.x, _)))
        
        case (East, x, _) if x == width - 1 => Left(0)
        case (East, x, _) if x == width - 2 => Left(1)
        case (East, _, _) => Right(Range(tree.x + 1 , width).toList.map((_, tree.y)))
        
        case (West, 0 ,_) => Left(0)
        case (West, 1 ,_) => Left(1)
        case (West, _ ,_) => Right(Range.inclusive(tree.x - 1, 0, -1).toList.map((_, tree.y)))
      }) match {
        case Left(0) => None
        case Left(s) => Some(s)
        case Right(path) => {
          var remaining = path
          var more = remaining.nonEmpty
          var visible = 0
          while (more) {
            visible += 1
            val (x, y) = remaining.head
            val height = forest.heightAt(x, y)
            remaining = remaining.tail
            more = tree.height > height && remaining.nonEmpty
            //println(s"${tree} => x=${x} y=${y} height=${height} more=${more}")
          }
  
          if (visible > 0) Some(visible) else None
        }
      }
      
    }
    
    def scenicScore(tree: Tree, forest: Forest): Int = {
      // Map our results, shortcutting if any are zero
      { for {
          northView <- viewFrom(tree, North, forest)
          southView <- viewFrom(tree, South, forest)
          eastView <- viewFrom(tree, East, forest)
          westView <- viewFrom(tree, West, forest)
        } yield (northView * southView * eastView * westView)
      } getOrElse 0
    }
    
    def score: Int = {
      var maxScore = 0
      var count = 0
      val forest = Forest(grid.map(_.map(_.height)))
      val trees = grid.flatten
      
      trees.zipWithIndex.map({ case (tree, i) =>
        count += 1
        val (d, s) = Time.timing {
          val score = scenicScore(tree, forest)
          maxScore = Math.max(score, maxScore)
          score
        }
        //println(s"${tree} (${i} of ${forest.size}) with score ${s} (max=${maxScore}, duration=${Time.prettyDuration(d)})")
        s
      }).max
    }
    
    def answer: String = s"${score}"
  }
}
