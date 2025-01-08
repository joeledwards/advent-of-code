package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.buzuli.collections.{FlipList, FlipListNode}

import java.util
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day9 extends AdventDay(9) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution1"
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    s"$solution2"
  }


  case class Block(
    fileId: Option[Long],
  )

  sealed trait BlockGroup {
    def length: Long

    lazy val blocks: List[Block] = this match {
      case File(id, length)  => List.fill(length.toInt)(Block(Some(id)))
      case FreeSpace(length) => List.fill(length.toInt)(Block(None))
    }
  }

  case class File(
    id: Long,
    length: Long,
  ) extends BlockGroup

  case class FreeSpace(
    length: Long,
  ) extends BlockGroup

  def toBlocks(blockGroups: List[BlockGroup]): List[Block] = blockGroups.flatMap(_.blocks)

  val filesAndFreeSpace: List[BlockGroup] = {
    lines
      .headOption
      .getOrElse("")
      .split("")
      .map(_.toLong)
      .toList
      .zipWithIndex
      .map({
        case (length, i) if i % 2 == 0 => File(i / 2, length)
        case (length, _) => FreeSpace(length)
      })
  }

  val contiguousBlocksChecksum: Long = {
    //println(blocks.map(b => b.fileId.map(_.toString).getOrElse(".")).mkString(""))

    val deque = new util.ArrayDeque[Block]
    toBlocks(filesAndFreeSpace).foreach(deque.addLast)

    def highestFileBlock(deque: util.Deque[Block]): Option[Block] = {
      var fileBlock: Option[Block] = None
      while (fileBlock.isEmpty && !deque.isEmpty) {
        val block = deque.removeLast()
        fileBlock = block.fileId.map(_ => block)
      }
      fileBlock
    }

    var contiguousFileBlocks: List[Long] = Nil
    while (!deque.isEmpty) {
      deque.removeFirst() match {
        case Block(Some(id)) => contiguousFileBlocks ::= id
        case Block(None)     => {
          highestFileBlock(deque) match {
            case Some(Block(Some(id))) => contiguousFileBlocks ::= id
            case _                     =>
          }
        }
      }
    }
    contiguousFileBlocks = contiguousFileBlocks.reverse

    //println(contiguousFileBlocks.map(_.toString).mkString(""))

    contiguousFileBlocks
      .zipWithIndex
      .map({ case (id, index) => index * id })
      .sum
  }

  lazy val contiguousFilesChecksum: Long = {
    //println(toBlocks(filesAndFreeSpace).map(b => b.fileId.map(_.toString).getOrElse(".")).mkString(""))

    val blockGroupsWorkingList: FlipList[BlockGroup] = FlipList(filesAndFreeSpace)

    def findEarliestAccommodatingFreeSpaceBefore(file: File): Option[FlipListNode[BlockGroup]] = {
      var keepSearching: Boolean = true
      var nextCandidate = blockGroupsWorkingList.left
      var result: Option[FlipListNode[BlockGroup]] = None

      while (keepSearching) {
        val (continue, outcome) = nextCandidate.map(_.value) match {
          case None                                             => (false, None)
          case Some(FreeSpace(length)) if file.length <= length => (false, nextCandidate)
          case Some(FreeSpace(_))                               => (true,  None)
          case Some(File(id, _)) if id == file.id               => (false, None)
          case Some(File(_, _))                                 => (true,  None)
        }

        keepSearching = continue

        if (keepSearching) {
          nextCandidate = nextCandidate.flatMap(_.right)
        } else {
          result = outcome
        }
      }

      result
    }

    var workingNode = blockGroupsWorkingList.right

    while (workingNode.nonEmpty) {
      var nextWorkingNode: Option[FlipListNode[BlockGroup]] = None

      workingNode.map(_.value) match {
        case None =>
        case Some(FreeSpace(_)) =>
        case Some(file @ File(_, fileLength)) => {
          findEarliestAccommodatingFreeSpaceBefore(file) match {
            case None =>
            case Some(freeNode) => freeNode.value match {
              case FreeSpace(freeLength) => {
                val leftOver = freeLength - fileLength
                val newFileNode = freeNode.addToRight(file)
                if (leftOver > 0) {
                  newFileNode.addToRight(FreeSpace(leftOver))
                }
                freeNode.remove()

                workingNode.map(_.addToRight(FreeSpace(fileLength)))
                nextWorkingNode = workingNode.flatMap(_.left)
                workingNode.foreach(_.remove())
              }
              case _ =>
            }
          }
        }
      }

      workingNode = nextWorkingNode orElse workingNode.flatMap(_.left)
    }

    val resolvedBlockGroups: List[BlockGroup] = blockGroupsWorkingList.iterator.toList
    val resolvedBlocks: List[Block] = toBlocks(resolvedBlockGroups)

    //println(resolvedBlocks.map(b => b.fileId.map(_.toString).getOrElse(".")).mkString(""))

    resolvedBlocks
      .map(_.fileId.getOrElse(0L))
      .zipWithIndex
      .map({ case (id, index) => index * id })
      .sum
  }

  lazy val solution1: Long = contiguousBlocksChecksum
  lazy val solution2: Long = contiguousFilesChecksum
}
