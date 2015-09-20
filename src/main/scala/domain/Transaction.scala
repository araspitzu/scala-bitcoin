package domain

import crypto.TransactionSignature.SigHashFlags
import domain.Numbers.CompactNumber
import domain.consensus.Script
import encoding.Parsing._
import CompactNumber._
import encoding.CommonParsersImplicits._
import encoding.Writing.ByteWritable


/**
 * Created by andrea on 11/05/15.
 */
case class Transaction(
     version:Long,                   //version is always 1
     nTxIn:CompactNumber,
     txIn:Array[TransactionInput],
     nTxOut:CompactNumber,
     txOut:Array[TransactionOutput],
     lockTime:Long
) extends ByteWritable {

  def isCoinbase = true //txIn.length == 1

  def byteFormat =
      uint32ByteFormatLE(version) ++
      nTxIn.byteFormat ++
      txIn.foldRight[Array[Byte]](Array.emptyByteArray)((in, acc) => in.byteFormat ++ acc) ++
      nTxOut.byteFormat ++
      txOut.foldRight[Array[Byte]](Array.emptyByteArray)((in, acc) => in.byteFormat ++ acc) ++
      uint32ByteFormatLE(lockTime)

  def hashForSignature(inputIndex: Int, scriptBytes: Array[Byte], sigFlag: Byte) = {
    val transactionInput = txIn(inputIndex).copy(signatureScript = Script(scriptBytes))



  }
}


object Transaction {

   implicit val transactionIsByteReadable = new {} with ByteReadable[Transaction]{
     def read(bytes: Array[Byte], offset: Int) = for {
        (vers,used) <- parse[Long](bytes,offset)(uint32ByteReaderLE).withOffset
        (ntxin,used1) <- parse[CompactNumber](bytes,offset + used).withOffset
        (txin,used2) <- parseList[TransactionInput](bytes,offset + used + used1, ntxin.intValue).withOffset
        (ntxout,used3) <- parse[CompactNumber](bytes,offset + used + used1 + used2).withOffset
        (txout,used4) <- parseList[TransactionOutput](bytes,offset + used + used1 + used2 + used3, ntxout.intValue).withOffset
        locktime <- parse[Long](bytes,offset + used + used1 + used2 + used3 + used4)(uint32ByteReaderLE)
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
