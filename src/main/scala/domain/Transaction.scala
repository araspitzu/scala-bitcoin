package domain

import domain.Numbers.CompactNumber
import domain.consensus.ScriptObject._
import encoding.Parsing._
import CompactNumber._
import encoding.CommonByteConverters._
import encoding.EnrichedTypes._
import encoding.Writing.ByteWritable


/**
 * Created by andrea on 11/05/15.
 */
case class Transaction(
     version:Long,
     nTxIn:CompactNumber,
     inputs:Array[TransactionInput],
     nTxOut:CompactNumber,
     outputs:Array[TransactionOutput],
     lockTime:Long
) extends ByteWritable {

  //TODO
  def isCoinbase = true //txIn.length == 1

  def byteFormat =
      uint32ByteFormatLE(version) ++
      nTxIn.byteFormat ++
      inputs.byteFormat ++
      nTxOut.byteFormat ++
      outputs.byteFormat ++
      uint32ByteFormatLE(lockTime)

  /**
   * Calculates a signature hash from this transaction, that is, a hash of a simplified form of it. How exactly the transaction
   * is simplified is specified by the type and anyoneCanPay parameters.
   *
   * When signing a P2SH output the redeemScript should be the script encoded into the scriptSig field, for normal transactions,
   * it's the scriptPubKey of the output you're signing for.
    *
    * See reference client at src/script/interpreter.cpp line:1109
   *
   * @param inputIndex input the signature is being calculated for. Tx signatures are always relative to an input.
   * @param redeemScript the bytes that should be in the given input during signing.
   * @param sigHash Should be SigHash.ALL
   * @param anyoneCanPay should be false.
   */
  def hashForSignature(inputIndex: Int,
                       redeemScript: Array[Byte],
                       sigHash: SigHash.Value,
                       anyoneCanPay: Boolean): Array[Byte] = {

    if(inputIndex >= inputs.length)
      return uint256one

    if( (sigHash.id & 0x1f) == SigHash.SIGHASH_SINGLE.id )
      if(inputIndex >= outputs.length)
        return uint256one


    ???
  }


}


object Transaction {

   implicit val transactionIsByteReadable = new {} with ByteReadable[Transaction]{
     def merda(implicit pippo:String) = {

     }

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
          inputs = txin,
          nTxOut = ntxout,
          outputs = txout,
          lockTime = locktime
       )
   }

}
