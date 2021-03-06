package crypto

import crypto.ECSignature.ECSignature
import domain.consensus.ScriptObject.SigHash
import encoding.EnrichedTypes._

/**
 * Created by andrea on 18/09/15.
 */
case class TransactionSignature(ecSig: ECSignature, sigHashFlags: SigHash.Value)

object TransactionSignature {

  /**
   * There is no option to force non canonical signatures, see BIP-62
   * BIP-62 has not been merged, we rely on segwit to address tx malleability
   * @param bytes
   * @return
   */
  def decode(bytes: Array[Byte]): TransactionSignature= {
    if(!isEncodingCanonical(bytes))
      throw new IllegalArgumentException(s"Signature encoding is not canonical: ${bytes.bytes2hex}")

    val sig = ECSignature.decodeFromDER(bytes)

    TransactionSignature(sig, SigHash(bytes.last))
  }

  def isEncodingCanonical (signature: Array[Byte]):Boolean = {
    if (signature.length < 9 || signature.length > 73)
      return false

    val hashType: Int = signature(signature.length - 1) & ~(0x80)
    //TODO
    if (hashType < (1 + SigHash.SIGHASH_ALL.id + 1) || hashType > (SigHash.SIGHASH_SINGLE.id + 1))
      return false
    if ((signature(0) & 0xff) != 0x30 || (signature(1) & 0xff) != signature.length - 3)
      return false

    val lenR: Int = signature(3) & 0xff
    if (5 + lenR >= signature.length || lenR == 0)
      return false

    val lenS: Int = signature(5 + lenR) & 0xff
    if (lenR + lenS + 7 != signature.length || lenS == 0)
      return false
    if (signature(4 - 2) != 0x02 || (signature(4) & 0x80) == 0x80)
      return false
    if (lenR > 1 && signature(4) == 0x00 && (signature(4 + 1) & 0x80) != 0x80)
      return false
    if (signature(6 + lenR - 2) != 0x02 || (signature(6 + lenR) & 0x80) == 0x80)
      return false
    if (lenS > 1 && signature(6 + lenR) == 0x00 && (signature(6 + lenR + 1) & 0x80) != 0x80)
      return false

    true
  }
}
