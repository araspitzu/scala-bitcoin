package domain

import encoding.Parsing._
import encoding.CommonParsersImplicits._


/**
 * Created by andrea on 21/06/15.
 */
case class TransactionOutput(
   value:Long,  // int64_t   ->  number of sathoshis spent
   pk_script_length:CompactNumber,
   pk_script:Array[Byte]
)

object TransactionOutput {

  implicit val txOutByteReadable = new {} with ByteReadable[TransactionOutput] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[TransactionOutput] = for {
      (satoshis,used) <- parse[Long](bytes,offset).withOffset
      (scrLen,used1) <- parse[CompactNumber](bytes,offset + used).withOffset
      script <- parseList[Byte](bytes,offset + used + used1,scrLen match {
        case CompactInt(i) => i
        case CompactLong(l) => l.toInt
        case CompactBigInt(b) => b.toInt
      })
    } yield TransactionOutput(
      value = satoshis,
      pk_script_length = scrLen,
      pk_script = script.toArray
    )
  }

}
