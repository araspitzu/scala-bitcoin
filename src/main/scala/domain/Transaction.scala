package domain

import encoding.Parsing._
import domain.CompactNumber._
import encoding.CommonParsersImplicits._


/**
 * Created by andrea on 11/05/15.
 */
case class Transaction(
     version:Long,                   // uint32_t
     nTxIn:CompactNumber,            // compactSize uint
     txIn:List[Int],                 // Transaction inputs
     nTxOut:CompactNumber,           //
     txOut:List[Int],                //
     lockTime:Long                   // uint32_t
)

object Transaction {

   implicit val transactionIsByteReadable = new {} with ByteReadable[Transaction]{
     def read(bytes: Array[Byte], offset: Int) = for {
         vers <- parse[Long](bytes,offset)
         ntxin <- parse[CompactNumber](bytes,offset + 1)
         //txin = List.empty[Int]
         ntxout <- parse[CompactNumber](bytes,offset + 2)
         //txout = List.empty[Int]
         locktime <- parse[Long](bytes,offset + 3)
       } yield Transaction(
          version = vers,
          nTxIn = ntxin,
          txIn = List.empty,
          nTxOut = ntxout,
          txOut = List.empty,
          lockTime = locktime
       )
   }

}
