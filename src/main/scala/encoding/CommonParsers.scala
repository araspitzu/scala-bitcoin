package encoding

import encoding.Parsing.{ParseSuccess, ParseResult, ByteReadable}

/**
 * Created by andrea on 17/06/15.
 */
package object CommonParsers {

  implicit class HexString(hex:String){
    def hex2bytes = hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

  implicit val byteReadable = new {} with ByteReadable[Byte] {
    def read(bytes: Array[Byte], offset: Int):ParseResult[Byte] = ParseSuccess(
      result = bytes(offset),
      bytesUsed = 1
    )
  }

  /**
   *  Byte readers for common numeric types
   */
  implicit val uint8ByteReader = new {} with ByteReadable[Short] {
    def read(bytes: Array[Byte], offset: Int):ParseResult[Short] = ParseSuccess(
      result = parseUint8(bytes,offset).toShort,
      bytesUsed = 1
    )
  }

  implicit val uint16ByteReader = new {} with ByteReadable[Int] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Int] = ParseSuccess(
      result = parseUint16(bytes,offset),
      bytesUsed = 2
    )
  }

  implicit val uint32ByteReader = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result = parseUint32LE(bytes,offset),
      bytesUsed = 4
    )
  }

  implicit val uint64ByteReader = new {} with ByteReadable[BigInt] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[BigInt] = ParseSuccess(
      result = parseUint64LE(bytes,offset),
      bytesUsed = 8
    )
  }

  private def parseUint8(bytes: Array[Byte], offset: Int) = bytes(offset) & 0xff

  private def parseUint16(bytes: Array[Byte], offset: Int):Int = ((bytes(offset) & 0xff) << 8) | bytes(offset + 1) & 0xff

  private def parseUint32LE(bytes: Array[Byte], offset: Int): Long = {
    ((bytes(offset + 3) & 0xffL) << 0) |
      ((bytes(offset + 2) & 0xffL) << 8) |
      ((bytes(offset + 1) & 0xffL) << 16) |
      ((bytes(offset + 0) & 0xffL) << 24)
  }

  private def parseUint64LE(bytes: Array[Byte],offset:Int):BigInt = {
    BigInt(((bytes(offset + 1) & 0xffL) << 0) |
      ((bytes(offset + 2) & 0xffL) << 8) |
      ((bytes(offset + 3) & 0xffL) << 16) |
      ((bytes(offset + 4) & 0xffL) << 24) |
      ((bytes(offset + 5) & 0xffL) << 32) |
      ((bytes(offset + 6) & 0xffL) << 40) |
      ((bytes(offset + 7) & 0xffL) << 48) |
      ((bytes(offset + 0) & 0xffL) << 56))
  }

}
