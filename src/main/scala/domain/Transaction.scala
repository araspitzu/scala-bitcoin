package domain

import encoding.Parsing._
import domain.CompactNumber._
import encoding.CommonParsersImplicits._


/**
 * Created by andrea on 11/05/15.
 */
case class Transaction(
     version:Long,
     nTxIn:CompactNumber,
     txIn:List[TransactionInput],
     nTxOut:CompactNumber,
     txOut:List[TransactionOutput],
     lockTime:Long
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
         (txout,used3) <- parseList[TransactionOutput](bytes,offset + used + used1 + used2 + ntxout.originalSize, ntxout match {
           case CompactInt(i) => i
           case CompactLong(l) => l.toInt
           case CompactBigInt(b) => b.toInt
         }).withOffset
         locktime <- parse[Long](bytes,offset + used + used1 + used2 + used3)
       } yield Transaction(
          version = vers,
          nTxIn = ntxin,
          txIn = txin,
          nTxOut = ntxout,
          txOut = txout,
          lockTime = locktime
       )
   }

}
