package domain.Numbers

import encoding.Parsing._
import encoding.CommonParsersImplicits._

/**
 * Created by andrea on 08/08/15.
 */
case class MPINumber(value:BigInt)

object MPINumber {


  val mpiByteReadable = new {} with ByteReadable[MPINumber] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[MPINumber] = for {
      length <- parse[Long](bytes, offset)
      mpi <- {
        //ParseSuccess has byteUsed = 0 because we are using 0 additional bytes other than
        //those used for the length itself, they will be added by the parsing composition
        if(length == 0) return ParseSuccess(MPINumber(BigInt(0)),0)

        //FIXME possible precision loss
        val data = bytes.slice(offset + 4,offset + 4 + length.toInt)
        val isNegative = (data(0) & 0x80) == 0x80
        ParseSuccess(
          result = MPINumber(BigInt(data)),
          bytesUsed = 4 + data.length
        )
      }
    } yield mpi
  }
}
