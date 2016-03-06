package domain

import domain.consensus.Script
import domain.Numbers.CompactNumber
import encoding.Parsing._
import encoding.CommonByteConverters._
import encoding.Writing.ByteWritable


/**
 * Created by andrea on 21/06/15.
 */
case class TransactionOutput(
   value:Long,  // int64_t   ->  number of sathoshis spent
   pkScriptLength:CompactNumber,
   pkScript:Script
) extends ByteWritable {

  override def byteFormat = int64ByteFormatLE(value) ++ pkScriptLength.byteFormat ++ pkScript.byteFormat

}

object TransactionOutput {

  implicit val txOutByteReadable = new {} with ByteReadable[TransactionOutput] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[TransactionOutput] = for {
      (satoshis,used) <- parse[Long](bytes,offset)(int64ByteReaderLE).withOffset
      scrLen <- parse[CompactNumber](bytes,offset + used)
      scriptData <- parseBytes(bytes,offset + used + scrLen.originalSize ,scrLen.intValue)
    } yield TransactionOutput(
      value = satoshis,
      pkScriptLength = scrLen,
      pkScript = Script(scriptData)
    )
  }

}
