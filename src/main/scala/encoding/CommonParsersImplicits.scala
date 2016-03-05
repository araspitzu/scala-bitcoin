package encoding

import Parsing.{ParseSuccess, ParseResult, ByteReadable}
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 17/06/15.
 */
package object CommonParsersImplicits {

  val alphabet:Array[Char] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toCharArray
  val encodedZero:Char = alphabet.head

  implicit class EnrichedArray[T <: ByteWritable](array:Array[T]) extends ByteWritable {
    override def byteFormat: Array[Byte] = {
      array.foldRight[Array[Byte]](Array.emptyByteArray)((tx, acc) => tx.byteFormat ++ acc )
    }
  }

  implicit class ByteVector(bytes: Array[Byte]) {

    def bytes2hex: String = bytes.map("%02x".format(_)).mkString

    def toBase58 = base58encode(bytes)

  }

  implicit class HexString(hex:String){
    def hex2bytes:Array[Byte] = hex.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  implicit class RichByte(byte:Byte){
    def unsigned:Int = byte & 0xff
  }
  /**
   *
   * @param bytes
   * @return
   */
  private def base58encode(bytes: Array[Byte]):String = {
    if(bytes.length == 0)
      return ""

    //count leading zeros
    var zeros = bytes.count(_ == 0)

    //the resulting char array, upper bound
    val encoded = new Array[Char](bytes.length * 2)

    val copy = java.util.Arrays.copyOf(bytes, bytes.length)
    var outputStart = encoded.length
    var inputStart = zeros
    while(inputStart < bytes.length){
      outputStart -= 1
      encoded(outputStart) = alphabet(divmod(bytes, inputStart, 256, 58))
      if(bytes(inputStart) == 0)
        inputStart += 1
    }

    while (outputStart < encoded.length && encoded(outputStart) == encodedZero)
      outputStart += 1

    zeros -= 1
    while(zeros >= 0){
      outputStart -= 1
      encoded(outputStart) = encodedZero
      zeros -=  1
    }

    new String(encoded, outputStart, encoded.length - outputStart)
  }

  private def divmod(number: Array[Byte], firstDigit: Int, base: Int, divisor: Int) = {
    var remainder: Int = 0

    var i: Int = firstDigit
    while (i < number.length) {

      val digit: Int = number(i).toInt & 0xFF
      val temp: Int = remainder * base + digit
      number(i) = (temp / divisor).toByte
      remainder = temp % divisor

      i += 1

    }

    remainder.toByte
  }
  /**
   * Byte formatters for common types, uint[length]ByteFormat[bit_order]
   */
  def uint32ByteFormatBE(uint:Long):Array[Byte] = Array(
      0xff & (uint >> 24) toByte,
      0xff & (uint >> 16) toByte,
      0xff & (uint >> 8) toByte,
      0xff & uint toByte
  )

  def uint32ByteFormatLE(uint:Long):Array[Byte] = Array(
      0xff & uint toByte,
      0xff & (uint >> 8) toByte,
      0xff & (uint >> 16) toByte,
      0xff & (uint >> 24) toByte
  )

  def uint16ByteFormatLE(uint:Int):Array[Byte] = Array(
    0xff & uint toByte,
    0xff & (uint >> 8) toByte
  )

  def uint8ByteFormat(uint:Int):Array[Byte] = Array(0xff & uint toByte)

  def uint8ByteFormat(uint:Short):Array[Byte] = uint8ByteFormat(uint.toInt)

  def uint64ByteFormatLE(uint:BigInt):Array[Byte] = Array(
    0xff & uint toByte,
    0xff & (uint >> 8) toByte,
    0xff & (uint >> 16) toByte,
    0xff & (uint >> 24) toByte,
    0xff & (uint >> 32) toByte,
    0xff & (uint >> 40) toByte,
    0xff & (uint >> 48) toByte,
    0xff & (uint >> 56) toByte
  )

  def int64ByteFormatLE(uint:Long):Array[Byte] = Array(
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
   *  Byte readers for common numeric types, uint[length]ByteReader[bit_order]
   */
  implicit val uint8ByteReader = new {} with ByteReadable[Short] {
    def read(bytes: Array[Byte], offset: Int):ParseResult[Short] = ParseSuccess(
      result = (bytes(offset) & 0xff).toShort,
      bytesUsed = 1
    )
  }

  implicit val uint16ByteReaderLE = new {} with ByteReadable[Int] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Int] = ParseSuccess(
      result = bytes(offset) & 0xff | (bytes(offset + 1) & 0xff) << 8 ,
      bytesUsed = 2
    )
  }

  implicit val uint32ByteReaderLE = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result =
        (bytes(offset + 0) & 0xffL) << 0  |
        (bytes(offset + 1) & 0xffL) << 8  |
        (bytes(offset + 2) & 0xffL) << 16 |
        (bytes(offset + 3) & 0xffL) << 24  ,
      bytesUsed = 4
    )
  }

  val uint32ByteReaderBE = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result =
          (bytes(offset + 0) & 0xffL) << 24 |
          (bytes(offset + 1) & 0xffL) << 16 |
          (bytes(offset + 2) & 0xffL) << 8  |
          (bytes(offset + 3) & 0xffL) << 0  ,
      bytesUsed = 4
    )
  }

  val int64ByteReaderLE = new {} with ByteReadable[Long] {
    override def read(bytes: Array[Byte], offset:Int):ParseResult[Long] = ParseSuccess(
      result = java.lang.Long.parseLong(bytes.slice(offset,offset + 8).reverse.bytes2hex,16),
      bytesUsed = 8
    )
  }

  implicit val uint64ByteReaderLE = new {} with ByteReadable[BigInt] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[BigInt] = ParseSuccess(
      result = BigInt(bytes.slice(offset,offset + 8).reverse.bytes2hex, 16),
      bytesUsed = 8
    )
  }

}
