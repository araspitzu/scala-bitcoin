package domain

import encoding.Parsing._
import encoding.CommonParsersImplicits._
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
      (satoshis,used) <- parse[Long](bytes,offset)(int64ByteReader).withOffset
      (scrLen,used1) <- parse[CompactNumber](bytes,offset + used).withOffset
      scriptData <- parseList[Byte](bytes,offset + used + used1,scrLen.intValue)
    } yield TransactionOutput(
      value = satoshis,
      pkScriptLength = scrLen,
      pkScript = Script(scriptData.toArray)
    )
  }

}
