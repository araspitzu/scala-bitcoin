package spikeTest


import domain._
import encoding.Parsing.ParseSuccess
import org.specs2.mutable.Specification

/**
 * Created by andrea on 25/05/15.
 */
class ParsingSpec extends Specification {

  val rawSignedTransaction = "01000000059277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99010000006b483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7ffffffff2cb46385bbdce98623916455ede6f88b44520df8d6a0faee744b9588533000c3000000006a473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587cffffffff7f5697c119c34c6e3c728c29d69d53b1040c9cfc7a8dcfaf2903463c02431f72000000006b483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039fffffffff9ffceab48b9cf61cc316b58c51fff325f2b70de3be5ff55c887b4e17bc5b3628010000006a47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1ffffffffbbccb3007f7ebef04fd8d3fe9f97601dee5e1f87589e31fc2e03b8ddcd8fd5c6020000006b483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56ffffffff0500c2eb0b000000001976a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac04edf900000000001976a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac00f90295000000001976a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac25bd3e06000000001976a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac1f351200000000001976a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac00000000"

  "The transacion serializer" should {

    "correctly read an uint8 as Int using the first byte from a byte array" in {
      import encoding.Parsing._
      import encoding.CommonParsers._

      val expected = 0xf2 //242
      val unit8bytes = Array(expected,0xff,0xff,0xff) map (_.toByte)

      parse[Short](unit8bytes,0) === ParseSuccess(242,1)

    }

    "parse an Outpoint" in {
      import encoding.Parsing._
      import domain.TransactionInput._

      val expectedIndex:Long = 3294967295L
      val uint32bytes = Array(0xc4,0x65,0x35,0xff) map (_.toByte)

      val hash:List[Int] = List(0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
                                0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff)

      val rawOutpoint:Array[Byte] = (hash.map(_.toByte) ++ (uint32bytes.map(_.toByte))).toArray

      val ris = parse[Outpoint](rawOutpoint,0)

      val outpoint:Outpoint = ris.get._1

      ris.get._2 === 36

      outpoint.hash.length === hash.length
      outpoint.index === expectedIndex

    }

    "use monadic operators to do parsing" in {
      import encoding.Parsing._
      import encoding.CommonParsers._

      val expected = 0xf2 //242
      val uint8bytes = Array(expected,0xff,0xff,0xff,0xff,0xff,0xff,0xf1,0xff,0xff,0xff) map (_.toByte)


      val sth = for {
        a <- parse[Int](uint8bytes,0)
        b <- parse[Int](uint8bytes,0)
        c <- parse[Int](uint8bytes,0)
        d <- parse[Int](uint8bytes,0)
        e <- parse[Int](uint8bytes,0)
      } yield {
        //System.err.println(s"a: $a , b: $b")
        a + b
      }

      //System.err.println(s"sth : $sth")
      (sth toOpt) must beSome
    }

//    "parse a compact number using the proper data type" in {
//
//      import domain.CompactNumber._
//      import encoding.Parsing._
//
////      val padding = Array(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff) map (_.toByte)
//
//      val expectedUint8:Int = 0xc8 //200
//      val uint8bytes = Array(expectedUint8.toByte)
//
//      val expectedUint16:Int = 515
//      val uint16bytes = Array(0xfd,0x2,0x3) map (_.toByte)
//
//      val expectedUint32:Long = 1243153423412L
//      val uint32bytes = Array(0xfe,0x0,0x1,0x2,0x3) map (_.toByte)
//
//      val expectedUint64:BigInt = BigInt(2 ^ 60 + 1)
//      val uint64bytes = Array(0xff,0x0,0x1,0x2,0x3,0x4,0x5,0x6,0x7) map (_.toByte)
//
//
//      val parsedUint8 = parse[CompactNumber](uint8bytes,0)
//      val parsedUint16 = parse[CompactNumber](uint16bytes,0)
//      val parsedUint32 = parse[CompactNumber](uint32bytes,0)
//      val parsedUint64 = parse[CompactNumber](uint64bytes,0)
//
//
//      parsedUint8 === ParseSuccess(CompactInt(expectedUint8))
//      parsedUint16 === ParseSuccess(CompactInt(expectedUint16))
//      parsedUint32 === ParseSuccess(CompactLong(expectedUint32))
//      parsedUint64 === ParseSuccess(CompactBigInt(expectedUint64))
//
//    }
//
    "parse an uint16 as CompactInt" in {
      import encoding.Parsing._
      import domain.CompactNumber._

      val expectedUint16:Int = 515
      val uint16bytes = Array(0x2,0x3) map (_.toByte)


      parse[CompactInt](uint16bytes,0) === ParseSuccess(CompactInt(expectedUint16),3)

    }

    "parse an uint32 as CompactLong" in {
      import encoding.Parsing._
      import domain.CompactNumber._

      val expectedUint32:Long = 3294967295L
      val uint32bytes = Array(0xc4,0x65,0x35,0xff) map (_.toByte)


      parse[CompactLong](uint32bytes,0) === ParseSuccess(CompactLong(expectedUint32),5)

    }

//    "parse an uint64 as CompactBigInt" in {
//      import encoding.Parsing._
//      import domain.CompactNumber._
//
//      /**
//       *  2^57 means in our byte array the 57th bit is set to 1
//       *
//       *  0000 0010 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000
//       *
//       *  adding 515 results in bitewise OR operation thus
//       *
//       *  0000 0010 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0000 | 0000 0010 | 0000 0011
//       *
//       */
//      val uint64bytes = Array(0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x2, 0x3) map (_.toByte)
//      val expectedUint64:BigInt = BigInt(uint64bytes)
//
//      parse[CompactBigInt](uint64bytes,0) === ParseSuccess(CompactBigInt(expectedUint64),8)
//
//    }.pendingUntilFixed("Fix uint64 parsing")

  }

}
