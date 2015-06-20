package spikeTest

import domain.TransactionInput
import org.specs2.mutable.Specification

/**
 * Created by andrea on 25/05/15.
 */
class ParsingSpec extends Specification {

  val rawSignedTransaction = "01000000059277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99010000006b483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7ffffffff2cb46385bbdce98623916455ede6f88b44520df8d6a0faee744b9588533000c3000000006a473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587cffffffff7f5697c119c34c6e3c728c29d69d53b1040c9cfc7a8dcfaf2903463c02431f72000000006b483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039fffffffff9ffceab48b9cf61cc316b58c51fff325f2b70de3be5ff55c887b4e17bc5b3628010000006a47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1ffffffffbbccb3007f7ebef04fd8d3fe9f97601dee5e1f87589e31fc2e03b8ddcd8fd5c6020000006b483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56ffffffff0500c2eb0b000000001976a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac04edf900000000001976a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac00f90295000000001976a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac25bd3e06000000001976a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac1f351200000000001976a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac00000000"

  //see http://bitcoin.stackexchange.com/questions/32628/redeeming-a-raw-transaction-step-by-step-example-required
  val rawTx = "0100000001be66e10da854e7aea9338c1f91cd489768d1d6d7189f586d7a3613f2a24d5396000000001976a914dd6cce9f255a8cc17bda8ba0373df8e861cb866e88acffffffff0123ce0100000000001976a914a2fd2e039a86dbcf0e1a664729e09e8007f8951088ac0000000001000000"

  "The transacion serializer" should {

    "parse an uint8 as Short" in {
      import encoding.Parsing._
      import encoding.CommonParsersImplicits._

      val expected = 0xf2 //242
      val unit8bytes = "f2".hex2bytes

      parse[Short](unit8bytes,0) === ParseSuccess(242,1)

    }

    "parse an uint16 as Int" in {
      import encoding.Parsing._
      import encoding.CommonParsersImplicits._

      val expectedUint16:Int = 515
      val uint16bytes = "0203".hex2bytes


      parse[Int](uint16bytes,0) === ParseSuccess(expectedUint16,2)

    }

    "parse an uint32 as Long" in {
      import encoding.Parsing._
      import encoding.CommonParsersImplicits._

      val expectedUint32:Long = 3294967295L
      val uint32bytes = "c46535ff".hex2bytes

      parse[Long](uint32bytes,0) === ParseSuccess(expectedUint32,4)

    }

//    "parse an uint64 as BigInt" in {
//      import encoding.Parsing._
//      import encoding.CommonParsersImplicits._
//
//      val uint64bytes = "0xc46535ff34f13f86".hex2bytes
//
//      parse[BigInt](uint64bytes,0) === ParseSuccess(BigInt(uint64bytes),8)
//
//    }

    "parse a CompactNumber using the proper type" in {
      import encoding.Parsing._
      import domain.CompactNumber
      import domain.CompactInt
      import domain.CompactLong
      import encoding.CommonParsersImplicits._

      val expectedUint8 = 0xf2 //242
      val uint8bytes = "f2".hex2bytes

      val expectedUint16:Int = 515
      val uint16bytes = "fd0203".hex2bytes

      val expectedUint32:Long = 3294967295L
      val uint32bytes = "fec46535ff".hex2bytes


      parse[CompactNumber](uint8bytes,0) === ParseSuccess(CompactInt(expectedUint8),1)
      parse[CompactNumber](uint16bytes,0) === ParseSuccess(CompactInt(expectedUint16),3)
      parse[CompactNumber](uint32bytes,0) === ParseSuccess(CompactLong(expectedUint32),5)

    }

    "parse an Outpoint" in {
      import encoding.Parsing._
      import domain.Outpoint
      import domain.TransactionInput._
      import encoding.CommonParsersImplicits._

      val expectedIndex:Long = 3294967295L
      val uint32bytes = "c46535ff"
      val hash = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069"

      val rawOutpoint:Array[Byte] = (hash ++ uint32bytes).hex2bytes

      val ris = parse[Outpoint](rawOutpoint,0)

      val outpoint:Outpoint = ris.get._1

      ris.get._2 === 36

      outpoint.hash.length === hash.length / 2
      outpoint.index === expectedIndex

    }

    "parse a TransactionInput" in  {
      import encoding.CommonParsersImplicits._
      import encoding.Parsing._
      import domain.TransactionInput._


      val rawOutpoint = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069c46535ff"
      val scrLen = "0c"
      val script = "b2c3ffb2c3ffb2c3ffb2c3efb2c3ffb2c3ffb2c3ffb2c3ff"
      val seq = "c46535ff"

      val rawTxIn = (rawOutpoint ++ scrLen ++ script ++ seq).hex2bytes

      val ris =  parse[TransactionInput](rawTxIn,0)

      System.err.println(ris)

      ris.get._1.previousOutput.index === 3294967295L
    }

  }

}
