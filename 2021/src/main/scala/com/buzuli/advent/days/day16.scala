package com.buzuli.advent.days

import com.buzuli.advent.{AdventContext, AdventDay}
import com.sun.org.apache.xerces.internal.impl.dv.util.HexBin

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object day16 extends AdventDay(16) {
  override def puzzles(implicit ec: ExecutionContext): List[AdventContext => Future[String]] = {
    List(puzzle1, puzzle2)
  }

  def puzzle1(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val inputLines = lines
    val decoder = Decoder(inputLines)
    val packet = decoder.packet
    val versionSum = packet.versionSum
    versionSum.toString
  }

  def puzzle2(context: AdventContext)(implicit ec: ExecutionContext): Future[String] = Future {
    val inputLines = lines
    val decoder = Decoder(inputLines)
    val packet = decoder.packet
    val value = packet.value
    value.toString
  }

  case class Decoder(lines: List[String]) {
    lazy val packetBytes: Array[Byte] = HexBin.decode(hexPacket)

    lazy val hexPacket: String = {
      lines
        .filter(_.nonEmpty)
        .head
    }

    var nextBit = 0

    lazy val packet: Packet = parsePacket()
    lazy val bitString: String = {
      nextBit = 0 // Reset so we get every bit
      val sb = new StringBuilder
      for (_ <- 0 until (packetBytes.length * 8)) {
        sb.append(getBit())
      }
      nextBit = 0 // Reset so we can still use the decoder
      sb.toString
    }

    private def parsePacket(): Packet = {
      val version = getLong(3)
      val typeId = getLong(3)

      typeId match {
        case 4 => { // Literal
          var literal = 0L

          // Consume value segments
          var hasMore = true
          while (hasMore) {
            hasMore = getLong(1) == 1L
            val quartet: Long = getLong(4)
            literal = (literal << 4) | quartet
          }
          Packet.literal(version, literal)
        }

        case opCode => { // Operator
          var subPackets: List[Packet] = Nil
          val lengthTypeId = getLong(1)

          if (lengthTypeId == 0) {
            var bytesConsumed = 0
            var remainingSubPacketBytes = getLong(15)
            while (remainingSubPacketBytes > 0) {
              val startBit = nextBit
              subPackets = parsePacket() :: subPackets
              val endBit = nextBit
              val consumed = endBit - startBit
              bytesConsumed += consumed
              remainingSubPacketBytes -= consumed
            }
          } else {
            var remainingSubPackets = getLong(11)
            while (remainingSubPackets > 0) {
              subPackets = parsePacket() :: subPackets
              remainingSubPackets -= 1
            }
          }

          Packet.operator(version, typeId, subPackets.reverse)
        }
      }
    }

    private def peekBit(): Long = {
      val packetOffset = nextBit / 8
      val shift = 7 - nextBit % 8
      (packetBytes(packetOffset) >> shift) & 0x01
    }

    private def getBit(): Long = {
      val bit = peekBit()
      nextBit += 1
      bit
    }

    private def getLong(bitCount: Int): Long = {
      var value = 0L
      for (shift <- (0 until bitCount).reverse) {
        value = value | (getBit() << shift)
      }
      value
    }
  }

  sealed abstract class Packet(val typeId: Long) {
    def value: Long
    def version: Long
    def versionSum: Long = version
  }

  trait Operator {
    this: Packet =>

    def subPackets: List[Packet]
    override def versionSum: Long = version + subPackets.map(_.versionSum).sum
  }

  case class Sum(version: Long, subPackets: List[Packet]) extends Packet(0) with Operator {
    override def value: Long = subPackets.map(_.value).sum
  }
  case class Product(version: Long, subPackets: List[Packet]) extends Packet(1) with Operator {
    override def value: Long = subPackets.map(_.value).product
  }
  case class Minimum(version: Long, subPackets: List[Packet]) extends Packet(2) with Operator {
    override def value: Long = subPackets.map(_.value).min
  }
  case class Maximum(version: Long, subPackets: List[Packet]) extends Packet(3) with Operator {
    override def value: Long = subPackets.map(_.value).max
  }
  case class Literal(version: Long, value: Long) extends Packet(4)
  case class Greater(version: Long, subPackets: List[Packet]) extends Packet(5) with Operator {
    override def value: Long = subPackets match {
      case a :: b :: Nil if a.value > b.value => 1
      case _ => 0
    }
  }
  case class Lesser(version: Long, subPackets: List[Packet]) extends Packet(6) with Operator {
    override def value: Long = subPackets match {
      case a :: b :: Nil if a.value < b.value => 1
      case _ => 0
    }
  }
  case class Equal(version: Long, subPackets: List[Packet]) extends Packet(7) with Operator {
    override def value: Long = subPackets match {
      case a :: b :: Nil if a.value == b.value => 1
      case _ => 0
    }
  }

  object Packet {
    def literal(version: Long, value: Long): Packet = Literal(version, value)
    def operator(version: Long, id: Long, subPackets: List[Packet]): Packet = id match {
      case 0 => Sum(version, subPackets)
      case 1 => Product(version, subPackets)
      case 2 => Minimum(version, subPackets)
      case 3 => Maximum(version, subPackets)
      case 5 => Greater(version, subPackets)
      case 6 => Lesser(version, subPackets)
      case 7 => Equal(version, subPackets)
    }
  }

  lazy val s1 = List("D2FE28")
  lazy val s2 = List("38006F45291200")
  lazy val s3 = List("EE00D40C823060")

  lazy val s4 = List("8A004A801A8002F478") // this packet has a version sum of 16.
  lazy val s5 = List("620080001611562C8802118E34") // This packet has a version sum of 12.
  lazy val s6 = List("C0015000016115A2E0802F182340") // This packet has a version sum of 23.
  lazy val s7 = List("A0016C880162017C3686B18A3D4780") // This packet has a version sum of 31.

  lazy val s8 = List("C200B40A82") // finds the sum of 1 and 2, resulting in the value 3.
  lazy val s9 = List("04005AC33890") // finds the product of 6 and 9, resulting in the value 54.
  lazy val s10 = List("880086C3E88112") // finds the minimum of 7, 8, and 9, resulting in the value 7.
  lazy val s11 = List("CE00C43D881120") // finds the maximum of 7, 8, and 9, resulting in the value 9.
  lazy val s12 = List("D8005AC2A8F0") // produces 1, because 5 is less than 15.
  lazy val s13 = List("F600BC2D8F") // produces 0, because 5 is not greater than 15.
  lazy val s14 = List("9C005AC2F8F0") // produces 0, because 5 is not equal to 15.
  lazy val s15 = List("9C0141080250320F1802104A08") // produces 1, because 1 + 3 = 2 * 2.
}