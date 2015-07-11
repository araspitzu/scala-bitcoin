package spikeTest

import domain._
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
import encoding.Parsing._
import domain.TransactionInput._

/**
 * Created by andrea on 7/11/15.
 */
class EncodingSpec extends Specification {

  "Domain objects" should {

    "encode an uint8 into byte" in new UnsignedIntegerScope {

      val bytes = uint8ByteFormat(uint8)

      parse[Short](bytes.toArray,0) === ParseSuccess(uint8,1)

    }

    "encode an uint16 into byte in big endian order" in new UnsignedIntegerScope {

      val bytes = uint16ByteFormatBE(uint16)

      parse[Int](bytes.toArray,0) === ParseSuccess(uint16,2)

    }

    "encode an uint32 into byte in little endian order" in new UnsignedIntegerScope {

      val bytes = uint32ByteFormatLE(uint32)

      parse[Long](bytes.toArray,0)(uint32ByteReaderLE) === ParseSuccess(uint32,4)

    }

    "encode an uint32 into byte in big endian order" in new UnsignedIntegerScope {

      val bytes = uint32ByteFormatBE(uint32)

      parse[Long](bytes.toArray,0)(uint32ByteReaderBE) === ParseSuccess(uint32,4)
    }

    "encode an uint64 into byte in little endian order" in new UnsignedIntegerScope {

      val bigInt = BigInt(uint64decimal,10)
      val bytes = uint64ByteFormatBE(bigInt)

      parse[BigInt](bytes.toArray,0) === ParseSuccess(bigInt,8)

    }

    "encode an int64 into byte in little endian order" in new UnsignedIntegerScope {

      val bytes = int64ByteFormatLE(int64)

      parse[Long](bytes.toArray,0)(int64ByteReader) === ParseSuccess(int64,8)

    }

    "encode a CompactNumber into byte" in new CompactNumberScope {

      val short12bytes = compactShort12.byteFormat
      val int515bytes = compactInt515.byteFormat
      val long3294967295bytes = compactLong3294967295.byteFormat
      val bigInt14151776774302809990bytes = compactBigInt14151776774302809990.byteFormat

      parse[CompactNumber](short12bytes.toArray,0) === ParseSuccess(compactShort12,1)
      parse[CompactNumber](int515bytes.toArray,0) === ParseSuccess(compactInt515,3)
      parse[CompactNumber](long3294967295bytes.toArray,0) === ParseSuccess(compactLong3294967295,5)
      parse[CompactNumber](bigInt14151776774302809990bytes.toArray,0) === ParseSuccess(compactBigInt14151776774302809990,9)

    }

    "encode an Outpoint into byte" in new CompactNumberScope {

      val outpoint = Outpoint(
        hash = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069".hex2bytes,
        12345667L
      )

      parse[Outpoint](outpoint.byteFormat.toArray,0) === ParseSuccess(outpoint,36)

    }


  }

}
