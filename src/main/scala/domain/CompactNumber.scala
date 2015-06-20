package domain

import encoding.CommonParsers
import encoding.Parsing._
import encoding.CommonParsers._
/**
 * Created by andrea on 16/05/15.
 */
sealed trait CompactNumber{

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
      parse[Long](bytes,offset) match {
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
      val (first:Short,byteUsed:Int) = CommonParsers.uint8ByteReader.read(bytes,offset) match {
        case ParseSuccess(i,used) => (i,used)
        case ParseFailure(e,t) => throw t.get
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