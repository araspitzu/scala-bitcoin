package serializationSpecs

import domain.Numbers._
import domain._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import encoding.CommonParsersImplicits._
import encoding.Parsing._

/**
 * Created by andrea on 25/05/15.
 */
trait UnsignedIntegerScope extends Scope {

  val uint8 = 12
  val uint8bytes = "0c"

  val uint16:Int = 515
  val uint16bytes = "0302"

  val uint32:Long = 3294967295L
  val uint32bytesLE = "ff3565c4"

  val uint64bytesLE = "863ff134ff3565c4"
  val uint64bytesBE = "c46535ff34f13f86"
  val uint64decimal = "14151776774302809990"

  val int64bytes = "404b4c0000000000"
  val int64:Long = 5000000L //  0.05 BTC

}

trait CompactNumberScope extends UnsignedIntegerScope {

  val compactShort12 = CompactInt(uint8)
  val compactShort12bytes = uint8bytes

  val compactInt515 = CompactInt(uint16)
  val compactInt515bytes = "fd"+uint16bytes

  val compactLong3294967295 = CompactLong(uint32)
  val compactLong3294967295bytes = "fe"+uint32bytesLE

  val compactBigInt14151776774302809990 = CompactBigInt(BigInt(uint64decimal,10))
  val compactBigInt14151776774302809990bytes = "ff"+uint64bytesLE

}

class ParsingSpec extends Specification {

  val rawTx = "01000000059277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99010000006b483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7ffffffff2cb46385bbdce98623916455ede6f88b44520df8d6a0faee744b9588533000c3000000006a473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587cffffffff7f5697c119c34c6e3c728c29d69d53b1040c9cfc7a8dcfaf2903463c02431f72000000006b483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039fffffffff9ffceab48b9cf61cc316b58c51fff325f2b70de3be5ff55c887b4e17bc5b3628010000006a47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1ffffffffbbccb3007f7ebef04fd8d3fe9f97601dee5e1f87589e31fc2e03b8ddcd8fd5c6020000006b483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56ffffffff0500c2eb0b000000001976a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac04edf900000000001976a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac00f90295000000001976a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac25bd3e06000000001976a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac1f351200000000001976a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac00000000"

  "The transacion serializer" should {

    "parse an uint8 as Short" in new UnsignedIntegerScope{

      parse[Short](uint8bytes) === ParseSuccess(uint8,1)

    }

    "parse an uint16 as Int" in new UnsignedIntegerScope{

      parse[Int](uint16bytes) === ParseSuccess(uint16,2)

    }

    "parse an uint32 in little endian order as Long" in new UnsignedIntegerScope {

      parse[Long](uint32bytesLE)(uint32ByteReaderLE) === ParseSuccess(uint32,4)

    }

    "parse an uint64 as BigInt" in new UnsignedIntegerScope {

      parse[BigInt](uint64bytesLE) === ParseSuccess(BigInt(uint64decimal,10),8)

    }

    "parse an int64 as Long" in new UnsignedIntegerScope {

      parse[Long](int64bytes)(int64ByteReaderLE) === ParseSuccess(int64,8)

    }

    "parse a CompactNumber using the proper type" in new CompactNumberScope{

      parse[CompactNumber](compactShort12bytes) === ParseSuccess(compactShort12,1)
      parse[CompactNumber](compactInt515bytes) === ParseSuccess(compactInt515,3)
      parse[CompactNumber](compactLong3294967295bytes) === ParseSuccess(compactLong3294967295,5)
      parse[CompactNumber](compactBigInt14151776774302809990bytes) === ParseSuccess(compactBigInt14151776774302809990,9)

    }

    "parse an Outpoint" in new UnsignedIntegerScope {
      import domain.TransactionInput._

      val expectedIndex = uint32
      val hash = "9277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99"

      val ParseSuccess(outpoint,used) = parse[Outpoint](hash ++ uint32bytesLE)

      used === 36
      outpoint.hash.length === hash.length / 2
      outpoint.index === expectedIndex

    }

    "parse a TransactionInput" in new CompactNumberScope {

      val rawOutpoint = s"86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069$uint32bytesLE"
      val script = "b2c3ffb2c3ffb2c3ffb2c3ff"
      val scrLen = compactShort12bytes

      val hexTxIn = rawOutpoint ++ scrLen ++ script ++ uint32bytesLE
      val expectedLength = 36 + 1 + 12 + 4

      val ParseSuccess(txIn,txInLength) =  parse[TransactionInput](hexTxIn)

      txIn.previousOutput.index === uint32
      txIn.sequence === uint32
      txIn.scriptLength === compactShort12
      txInLength === expectedLength

    }

    "parse a TransactionOutput" in new CompactNumberScope {

      val rawTxOut = int64bytes ++ compactShort12bytes ++ "76a914e7b369882ebdd4b5907012e3f3e98ab0fbafd01888ac"

      val ParseSuccess(txOut,_) = parse[TransactionOutput](rawTxOut.hex2bytes, 0)

      txOut.value === int64
      txOut.pkScriptLength === compactShort12
      txOut.pkScript.length === 12

    }

    "parse a Transaction" in {

      val ParseSuccess(tx,txLength) = parse[Transaction](rawTx)

      tx.version === 1L
      tx.nTxIn === CompactInt(5)
      tx.txIn.map(_.sequence) forall (_ == 4294967295L)
      tx.nTxOut === CompactInt(5)
      tx.txOut(0).value === 200000000L
      tx.lockTime === 0L

    }

    "parse transaction 58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e" in {

      val hex = scala.io.Source.fromFile(getClass.getResource("/58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e.hex").getFile).mkString

      val ParseSuccess(tx,_) = parse[Transaction](hex)

      tx.version === 1
      tx.nTxIn === CompactInt(3)
      tx.nTxOut === CompactInt(2)
      tx.txIn.length === 3
      tx.txOut.length === 2

      tx.txIn.head.previousOutput.hash.bytes2hex === "c4b4dda5204f1796e65a5d740b87d2c4540c2a6bf85fd7e779ad4b789126b94d"

      tx.txOut.head.value === 124000000

    }

    "parse a BlockHeader" in {

      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"

      val ParseSuccess(header,length) = parse[BlockHeader](rawBlockHeader)

      header.version === 2
      header.time === 1415239972 //Unix timestamp
      length === 80
    }

    "parse a Block" in {
      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"
      val nTx = "03"
      val tx = rawTx

      val rawBlock = rawBlockHeader ++ nTx ++ tx ++ tx ++ tx

      val ParseSuccess(block, blockSize) = parse[Block](rawBlock)

      block.header.version === 2
      block.header.time === 1415239972L
      block.numTransaction === CompactInt(3)
      block.txs.length === 3
      block.txs forall (_.version == 1)
      block.txs forall (_.nTxOut === CompactInt(5))
      blockSize === rawBlock.length / 2

    }

    "parse block 000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9" in {

      val hex = scala.io.Source.fromFile(getClass.getResource("/000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9.hex").getFile).mkString

      val ParseSuccess(block,used) = parse[Block](hex)

      used === hex.length / 2
      block.numTransaction === CompactInt(1031)
      block.txs.length === 1031

      block.header.time === 1432723472
      block.header.version === 2
      block.header.prevHeaderHash.bytes2hex === "66191da95594aeda1a98a19ff054a88a510754e2a4d93e0a0000000000000000"
      block.header.merkleRootHash.bytes2hex === "8485ae797312b2cb37dfb1aac11d7c5ad9dd84364bbe26ffa781853996587d9b"

      val coinbase = block.txs.head

      coinbase.nTxIn === CompactInt(1)
      coinbase.txIn.length === 1
      coinbase.txIn.head.previousOutput.hash forall (_ == 0)

    }
  }

}
