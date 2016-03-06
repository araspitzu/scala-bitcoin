package domain

import Conf.TxConfig
import crypto.Hash._
import encoding.EnrichedTypes._
/**
 * Created by andrea on 26/09/15.
 */
trait VersionedChecksummed {

  val bytes:Array[Byte]
  val version:Byte = TxConfig.VERSION.toByte

  /**
   * Returns the base-58 encoded String representation of this
   * object with version and checksum bytes.
   *
   * [1-byte version][data][4-byte checksum]
   */
  def toBase58:String = {

    val resultBytes = new Array[Byte](1 + bytes.length + 4)
    resultBytes(0) = version

    System.arraycopy(bytes, 0, resultBytes, 1, bytes.length)
    val checksum = sha256Twice(resultBytes, 0, bytes.length + 1)

    System.arraycopy(checksum, 0, resultBytes, bytes.length + 1, 4)
    resultBytes.toBase58
  }

}
