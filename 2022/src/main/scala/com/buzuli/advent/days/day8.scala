package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

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
  
  object p1 {
    val grid: Grid = lines.zipWithIndex.map { case (r, x) =>
      r.toCharArray.toList.zipWithIndex.map { case (h, y) =>
        Tree(x, y, h.toInt)
      }
    }
    
    val top: Grid = transposeGrid(grid)
    val left: Grid = grid
    val right: Grid = left.map(_.reverse)
    val bottom: Grid = top.map(_.reverse)
    
    val visible: Set[Tree] = {
      visibleTrees(top) | visibleTrees(left) | visibleTrees(right) | visibleTrees(bottom)
    }
    
    def answer: String = s"${visible.size}"
  }
  
  object p2 {
    def answer: String = ""
  }
}
