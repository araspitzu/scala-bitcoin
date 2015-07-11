package domain

import encoding.CommonParsersImplicits
import encoding.Parsing._
import encoding.CommonParsersImplicits._
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 16/05/15.
 */
sealed trait CompactNumber extends ByteWritable {

  /**
   * The number of byte originally used by this compact number in the transaction
   * See https://bitcoin.org/en/developer-reference#CompactInt
   * @return
   */
  def originalSize = this match {
    case CompactInt(i) => if(i < 253) 1 else 3
    case _:CompactLong => 5
    case _:CompactBigInt => 9
  }

  def intValue:Int = this match {
    case CompactInt(i) => i
    case CompactLong(l) => l.toInt
    case CompactBigInt(b) => b.toInt
  }

  def longValue:Long = this match {
    case CompactInt(i) => i.toLong
    case CompactLong(l) => l
    case CompactBigInt(b) => b.toLong
  }

  def bigIntValue:BigInt = this match {
    case CompactInt(i) => i
    case CompactLong(l) => l
    case CompactBigInt(b) => b
  }

  override def byteFormat:List[Byte] = this match {
    case CompactInt(i) if(i < 253) => uint8ByteFormat(i)
    case CompactInt(i) => 253.toByte :: uint16ByteFormatBE(i)
    case CompactLong(l) => 254.toByte :: uint32ByteFormatBE(l)
    case CompactBigInt(b) => 0xff.toByte :: uint64ByteFormatBE(b)
  }

}

case class CompactInt(value:Int) extends CompactNumber
case class CompactLong(value:Long) extends CompactNumber
case class CompactBigInt(value:BigInt) extends CompactNumber

object CompactNumber {

  implicit val compactIntByteReader = new {} with ByteReadable[CompactInt] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[CompactInt] = {
      parse[Int](bytes,offset) match {
        case ParseSuccess(result,used) => {
          val cint = CompactInt(result)
          ParseSuccess(cint,cint.originalSize)
        }
        case e:ParseFailure => e
      }
    }
  }

  implicit val compactLongByteReader = new {} with ByteReadable[CompactLong] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[CompactLong] = {
      parse[Long](bytes,offset)(uint32ByteReaderBE) match {
        case ParseSuccess(result,used) => {
          val clong = CompactLong(result)
          ParseSuccess(clong,clong.originalSize)
        }
        case e:ParseFailure => e
      }
    }
  }

  implicit val compactBigIntByteReader = new {} with ByteReadable[CompactBigInt] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[CompactBigInt] = {
      parse[BigInt](bytes,offset) match {
        case ParseSuccess(result,used) => {
          val cbint = CompactBigInt(result)
          ParseSuccess(cbint,cbint.originalSize)
        }
        case e:ParseFailure => e
      }
    }
  }

  implicit val compactNumberByteReader = new {} with ByteReadable[CompactNumber] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[CompactNumber] = {
      val (first:Short,byteUsed:Int) = CommonParsersImplicits.uint8ByteReader.read(bytes,offset) match {
        case ParseSuccess(i,used) => (i,used)
        case f:ParseFailure => f
      }

      if(first < 253)
        ParseSuccess(CompactInt(first.toInt), byteUsed)
      else if(first == 253)
        compactIntByteReader.read(bytes,offset + 1)
      else if(first == 254)
        compactLongByteReader.read(bytes,offset + 1)
      else
        compactBigIntByteReader.read(bytes,offset + 1)
    }
  }

}