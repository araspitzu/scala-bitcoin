package domain.Numbers

import encoding.Parsing._
import encoding.CommonParsersImplicits._
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 08/08/15.
 */
case class MPINumber(value:BigInt) extends ByteWritable {

  override def byteFormat:Array[Byte] = {
    if(value == 0) return Array(0x00.toByte,0x00.toByte,0x00.toByte,0x00.toByte)

    val isNegative = value.signum < 0
    val data = if(isNegative) value.underlying.negate.toByteArray else value.toByteArray

    uint32ByteFormatBE(data.length) ++ (if(isNegative) Array( (data.head| 0x80).toByte ) ++ data.tail else data)
  }

}

object MPINumber {


 implicit val mpiByteReadable = new {} with ByteReadable[MPINumber] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[MPINumber] = for {
      length <- parse[Long](bytes, offset)(uint32ByteReaderBE)
      //TODO put a limit here
      if(length < 30000)
      mpi <- {
        //ParseSuccess has byteUsed = 0 because we are using 0 additional bytes other than
        //those used for the length itself, they will be added by the parsing composition
        if(length == 0) return ParseSuccess(MPINumber(BigInt(0)),0)
        //FIXME possible precision loss
        val data = bytes.slice(offset + 4,offset + 4 + length.toInt)
        val isNegative = (data(0) & 0x80) == 0x80

        val number = if(isNegative)
          BigInt( Array( (data.head & 0x7f).toByte ) ++ data.tail )
        else
          BigInt(data)

        ParseSuccess(
          result = MPINumber(if(isNegative) number.underlying.negate else number),
          bytesUsed = data.length
        )
      }
    } yield mpi
  }
}
