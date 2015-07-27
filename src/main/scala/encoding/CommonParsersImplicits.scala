package encoding

import encoding.Parsing.{ParseSuccess, ParseResult, ByteReadable}

/**
 * Created by andrea on 17/06/15.
 */
package object CommonParsersImplicits {

  implicit class HexString(hex:String){
    def hex2bytes:Array[Byte] = hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString


  /**
   * Byte formatters for common types
   */
  def uint32ByteFormatBE(uint:Long):List[Byte] = List(
      0xff & (uint >> 24) toByte,
      0xff & (uint >> 16) toByte,
      0xff & (uint >> 8) toByte,
      0xff & uint toByte
  )

  def uint32ByteFormatLE(uint:Long):List[Byte] = List(
      0xff & uint toByte,
      0xff & (uint >> 8) toByte,
      0xff & (uint >> 16) toByte,
      0xff & (uint >> 24) toByte
  )

  def uint16ByteFormatBE(uint:Int):List[Byte] = List(
    0xff & (uint >> 8) toByte,
    0xff & uint toByte
  )

  def uint16ByteFormatLE(uint:Int):List[Byte] = List(
    0xff & uint toByte,
    0xff & (uint >> 8) toByte
  )

  def uint8ByteFormat(uint:Int):List[Byte] = List(0xff & uint toByte)

  def uint8ByteFormat(uint:Short):List[Byte] = uint8ByteFormat(uint.toInt)

  def uint64ByteFormatBE(uint:BigInt):List[Byte] = List(
    0xff & (uint >> 56) toByte,
    0xff & (uint >> 48) toByte,
    0xff & (uint >> 40) toByte,
    0xff & (uint >> 32) toByte,
    0xff & (uint >> 24) toByte,
    0xff & (uint >> 16) toByte,
    0xff & (uint >> 8) toByte,
    0xff & uint toByte
  )

  def int64ByteFormatLE(uint:Long):List[Byte] = List(
    0xff & uint toByte,
    0xff & (uint >> 8) toByte,
    0xff & (uint >> 16) toByte,
    0xff & (uint >> 24) toByte,
    0xff & (uint >> 32) toByte,
    0xff & (uint >> 40) toByte,
    0xff & (uint >> 48) toByte,
    0xff & (uint >> 56) toByte
  )


  /**
   *  Byte readers for common numeric types
   */
  implicit val byteReadable = new {} with ByteReadable[Byte] {
    def read(bytes: Array[Byte], offset: Int):ParseResult[Byte] = ParseSuccess(
      result = bytes(offset),
      bytesUsed = 1
    )
  }

  implicit val uint8ByteReader = new {} with ByteReadable[Short] {
    def read(bytes: Array[Byte], offset: Int):ParseResult[Short] = ParseSuccess(
      result = parseUint8(bytes,offset).toShort,
      bytesUsed = 1
    )
  }

  implicit val uint16ByteReader = new {} with ByteReadable[Int] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Int] = ParseSuccess(
      result = parseUint16LE(bytes,offset),
      bytesUsed = 2
    )
  }

  val uint32ByteReaderLE = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result = parseUint32LE(bytes,offset),
      bytesUsed = 4
    )
  }

  val int64ByteReader = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result = parseInt64LE(bytes,offset),
      bytesUsed = 8
    )
  }

  implicit val uint64ByteReader = new {} with ByteReadable[BigInt] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[BigInt] = ParseSuccess(
      result = parseUint64LE(bytes,offset),
      bytesUsed = 8
    )
  }

  private def parseUint8(bytes: Array[Byte], offset: Int) = bytes(offset) & 0xff

  private def parseUint16LE(bytes: Array[Byte], offset: Int):Int = bytes(offset) & 0xff | (bytes(offset + 1) & 0xff) << 8

  private def parseUint32LE(bytes: Array[Byte], offset: Int): Long = {
      ((bytes(offset + 0) & 0xffL) << 0)  |
      ((bytes(offset + 1) & 0xffL) << 8)  |
      ((bytes(offset + 2) & 0xffL) << 16) |
      ((bytes(offset + 3) & 0xffL) << 24)
  }

  private def parseInt64LE(bytes: Array[Byte], offset: Int): Long = java.lang.Long.parseLong(bytes2hex(bytes.slice(offset,offset + 8).reverse),16)

  private def parseUint64LE(bytes: Array[Byte],offset:Int):BigInt = BigInt(bytes2hex(bytes.slice(offset,offset + 8)), 16)

}
