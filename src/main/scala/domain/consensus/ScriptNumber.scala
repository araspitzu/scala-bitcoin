package domain.consensus

import java.math.BigInteger
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 05/09/15.
 */
case class ScriptNumber(int:BigInt) extends ByteWritable {

  override def byteFormat:Array[Byte] = {
    if(int == BigInt(0))
      return Array.emptyByteArray

    val isNegative =  int.signum < 0
    //
    val bytes = if(isNegative) int.bigInteger.negate().toByteArray else int.bigInteger.toByteArray
    // extra space to store the  sign bit
    val length = if ((bytes(0) & 0x80) == 0x80) bytes.length + 1 else bytes.length

    var result = if(length == bytes.length) bytes else Array(0x00.toByte) ++ bytes

    if (isNegative)
      result(0) = (result(0).toInt | 0x80).toByte

    result
  }

}

object ScriptNumber {

  //Bytes
  val SCRIPT_NUM_MAX_SIZE = 4

  def apply(bytes:Array[Byte], requireMinimal:Boolean):ScriptNumber = {
    require(bytes.length <= SCRIPT_NUM_MAX_SIZE, s"Script number overflow, expected length < $SCRIPT_NUM_MAX_SIZE , actual ${bytes.length}")

    //TODO there are more checks in Bitcoin Core

    apply(bytes)
  }


  private def apply(bytes:Array[Byte]):ScriptNumber = {
    if(bytes.isEmpty)
      return ScriptNumber(BigInt(0))

    val isNegative = (bytes(0) & 0x80) == 0x80

    val bigInt = if(isNegative) {
      bytes(0) = (bytes(0).toInt & 0x7f).toByte
      new BigInteger(bytes) negate
    } else {
      new BigInteger(bytes)
    }

    ScriptNumber(BigInt(bigInt))
  }

}
