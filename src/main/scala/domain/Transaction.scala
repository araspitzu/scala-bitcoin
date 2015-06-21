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
     txIn:List[TransactionInput],                 // Transaction inputs
     nTxOut:CompactNumber,           //
     txOut:List[Int],                //
     lockTime:Long                   // uint32_t
)

object Transaction {

   implicit val transactionIsByteReadable = new {} with ByteReadable[Transaction]{
     def read(bytes: Array[Byte], offset: Int) = for {
        (vers,used) <- parse[Long](bytes,offset).withOffset
        (ntxin,used1) <- parse[CompactNumber](bytes,offset + used).withOffset
        (txin,used2) <- parseList[TransactionInput](bytes,offset + used + used1, ntxin match {
           case CompactInt(i) => i
           case CompactLong(l) => l.toInt
           case CompactBigInt(b) => b.toInt
         }).withOffset
         ntxout <- parse[CompactNumber](bytes,offset + used + used1 + used2)
         //txout = List.empty[Int]
         locktime <- parse[Long](bytes,offset + 3)
       } yield Transaction(
          version = vers,
          nTxIn = ntxin,
          txIn = txin,
          nTxOut = ntxout,
          txOut = List.empty,
          lockTime = locktime
       )
   }

}
