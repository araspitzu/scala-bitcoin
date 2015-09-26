package domain

import crypto.CryptoInitialization
import crypto.Hash._
import encoding.CommonParsersImplicits._
/**
 * Created by andrea on 26/09/15.
 */
trait VersionedChecksummed {

  val bytes:Array[Byte]
  val version:Int

  /**
   * Returns the base-58 encoded String representation of this
   * object with version and checksum bytes.
   *
   * [1-byte version][data][4-byte checksum]
   */
  def toBase58 = {

    val resultBytes = new Array[Byte](1 + bytes.length + 4)
    resultBytes(0) = version.toByte

    System.arraycopy(bytes, 0, resultBytes, 1, bytes.length)
    val checksum = sha256Twice(resultBytes, 0, bytes.length + 1)

    System.arraycopy(checksum, 0, resultBytes, bytes.length + 1, 4)
    base58encode(resultBytes)
  }

}
