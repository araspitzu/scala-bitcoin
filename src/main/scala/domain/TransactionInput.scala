package domain

import encoding.Parsing._
import encoding.CommonParsers._
/**
 * Created by andrea on 13/06/15.
 */
case class TransactionInput (
   previousOutput:Outpoint,
   scriptLength:CompactNumber,
   signatureScript:Array[Byte],
   sequence:Long
)

case class Outpoint(
   hash:Array[Byte],
   index:Long
)

object TransactionInput {

  implicit val outpointByteReadable = new {} with ByteReadable[Outpoint] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Outpoint] = {
      parse[Long](bytes,offset + 32) match {
        case ParseSuccess(index,used) => ParseSuccess(
          result = Outpoint(
            hash = bytes.slice(offset,32),
            index
          ),
          bytesUsed = 32 + used
        )
        case e:ParseFailure => e
      }
    }
  }


  implicit val transactionInputByteReadable = new {} with ByteReadable[TransactionInput] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[TransactionInput] = for {
      (prevOut,used) <- parse[Outpoint](bytes,offset).withOffset
      (scrLen,used1) <- parse[CompactNumber](bytes,offset + used).withOffset
      (script,used2) <- parseList[Byte](bytes,offset + used + used1,scrLen match {
        case CompactInt(i) => i
        case CompactLong(l) => l.toInt
        case CompactBigInt(b) => b.intValue
      }).withOffset
      seqNum <- parse[Long](bytes,offset + used + used1 + used2)
    } yield TransactionInput(
       previousOutput = prevOut,
       scriptLength = scrLen,
       signatureScript = script.toArray,
       sequence = seqNum
     )
  }


}