package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}

import java.util
import scala.concurrent.{ExecutionContext, Future}

object day7 extends AdventDay(7) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }
  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p1.answer }
  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future { p2.answer }
  
  case class File(parent: Dir, name: String, size: Long)
  case class Dir(parent: Option[Dir], name: String) {
    val files: scala.collection.mutable.LinkedHashSet[File] = scala.collection.mutable.LinkedHashSet.empty
    val dirs: scala.collection.mutable.Map[String, Dir] = scala.collection.mutable.Map.empty
    
    def size: Long = files.map(_.size).sum + dirs.values.map(_.size).sum
    
    def addFile(f: File): Boolean = files.add(f)
    def addDir(d: Dir): Option[Dir] = dirs.put(d.name, d)
    
    def getDir(name: String): Option[Dir] = dirs.get(name)
  }
  
  def parseTerminalOutput(lines: List[String]): Dir = {
    val root = Dir(None, "/")
    var cwd = root
    lines foreach { line =>
      line match {
        case "$ cd /" => cwd = root
        case "$ cd .." => cwd = cwd.parent.getOrElse(cwd)
        case s"$$ cd ${dirName}" => {
          cwd.getDir(dirName) match {
            case Some(dir) => cwd = dir
            case None => {
              val newDir = Dir(Some(cwd), dirName)
              cwd.addDir(newDir)
              cwd = newDir
            }
          }
        }
        case "$ ls" =>
        case s"dir ${dirName}" => cwd.getDir(dirName) match {
          case None => cwd.addDir(Dir(Some(cwd), dirName))
          case Some(_) =>
        }
        case s"${size} ${fileName}" => cwd.addFile(File(cwd, fileName, size.toLong))
      }
    }
    root
  }
  
  object p1 {
    def dirSums(dirs: List[Dir], maxSizeToCount: Long): Long = {
      dirs.foldLeft(0L) { (acc, dir) =>
        val dirSize = dir.size match {
          case s if s <= maxSizeToCount => s
          case _ => 0
        }
        
        val subdirsSize = dirSums(dir.dirs.values.toList, maxSizeToCount)
  
        //println(s"${dir} => size=${dirSize} subdirsSize=${subdirsSize}")
        
        acc + dirSize + subdirsSize
      }
    }
    
    val root = parseTerminalOutput(lines)
    val smallSizesTotal = dirSums(List(root), 100000L)
    def answer: String = s"${smallSizesTotal}"
  }
  
  object p2 {
    val root = parseTerminalOutput(lines)
    val systemTotal = 70000000
    val systemUsed = root.size
    val freeSpaceNeeded = 30000000
    val startingSpaceFree = systemTotal - systemUsed
    val additionalSpaceNeeded = freeSpaceNeeded - startingSpaceFree
    println(s"systemTotal: ${systemTotal}")
    println(s"systemUsed: ${systemUsed}")
    println(s"freeSpaceNeeded: ${freeSpaceNeeded}")
    println(s"startingSpaceFree: ${startingSpaceFree}")
    println(s"additionalSpaceNeeded: ${additionalSpaceNeeded}")
    
    def dirSizes(dirs: List[Dir]): List[Long] = {
      dirs.foldLeft[List[Long]](Nil) { (acc, dir) =>
        (dir.size :: acc) ::: dirSizes(dir.dirs.values.toList)
      }
    }
    
    val toDeleteSize = dirSizes(List(root)).sorted.collectFirst {
      case size if size > additionalSpaceNeeded => size
    }
    
    def answer: String = s"${toDeleteSize}"
  }
}
