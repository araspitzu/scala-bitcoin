package domain

import domain.Numbers.CompactNumber
import domain.consensus.Script
import encoding.Parsing._
import encoding.CommonByteConverters._
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 13/06/15.
 */
case class TransactionInput (
   previousOutput:Outpoint,
   scriptLength:CompactNumber,
   signatureScript:Script,
   sequence:Long
) extends ByteWritable {

  def byteFormat =
    previousOutput.byteFormat ++
    scriptLength.byteFormat ++
    signatureScript.byteFormat ++
    uint32ByteFormatLE(sequence)

}

case class Outpoint(
   hash:Array[Byte],
   index:Long
) extends ByteWritable {

  override def byteFormat:Array[Byte] = hash ++ uint32ByteFormatLE(index)

}

object TransactionInput {

  implicit val outpointByteReadable = new {} with ByteReadable[Outpoint] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Outpoint] =  {
      parse[Long](bytes,offset + 32)(uint32ByteReaderLE) match {
        case ParseSuccess(index,used) => ParseSuccess(
          result = Outpoint(
            hash = bytes.slice(offset, offset + 32),
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
      (scriptData,used2) <- parseBytes(bytes,offset + used + used1,scrLen.intValue).withOffset
      seqNum <- parse[Long](bytes,offset + used + used1 + used2)(uint32ByteReaderLE)
    } yield TransactionInput(
       previousOutput = prevOut,
       scriptLength = scrLen,
       signatureScript = Script(scriptData),
       sequence = seqNum
     )
  }


}