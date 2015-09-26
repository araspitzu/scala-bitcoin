package serializationSpecs

import domain.consensus.Script
import domain.Numbers.{CompactInt, CompactNumber}
import domain._
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
import encoding.Parsing._
import domain.TransactionInput._
import crypto.Hash._

/**
 * Created by andrea on 7/11/15.
 */
class EncodingSpec extends Specification {

  "Domain objects" should {

    "encode bytes in base58" in {

      val helloWorldBytes = "Hello World".getBytes
      base58encode(helloWorldBytes) === "JxF12TrwUP45BMd"

      val bigIntBytes = BigInt(3471844090L).toByteArray
      base58encode(bigIntBytes) === "16Ho7Hs"

      val zeroBytes = Array(0.toByte)
      base58encode(zeroBytes) === "1"

      val zeroByte5times =  Array(0.toByte,0.toByte,0.toByte,0.toByte,0.toByte)
      base58encode(zeroByte5times) === "11111"

    }

    "encode bytes with checksum and version in base58" in {

      val helloWorld = "Hello World".getBytes

      val helloHash160 = hash160(helloWorld)

      val versionedChecksummed = new {} with VersionedChecksummed {
        val bytes = helloHash160
        val version = 111
      }

      versionedChecksummed.toBase58 === "mxqVBges55Hex1W4Ga1tQ8X2TegvkAac7v"

    }

    "encode an uint8 into byte" in new UnsignedIntegerScope {

      val bytes = uint8ByteFormat(uint8)

      parse[Short](bytes,0) === ParseSuccess(uint8,1)

    }

    "encode an uint16 into byte in little endian order" in new UnsignedIntegerScope {

      val bytes = uint16ByteFormatLE(uint16)

      parse[Int](bytes,0) === ParseSuccess(uint16,2)

    }

    "encode an uint32 into byte in little endian order" in new UnsignedIntegerScope {

      val bytes = uint32ByteFormatLE(uint32)

      parse[Long](bytes,0)(uint32ByteReaderLE) === ParseSuccess(uint32,4)

    }

    "encode an uint64 into byte in little endian order" in new UnsignedIntegerScope {

      val bigInt = BigInt(uint64decimal,10)
      val bytes = uint64ByteFormatLE(bigInt)

      parse[BigInt](bytes,0) === ParseSuccess(bigInt,8)

    }

    "encode an int64 into byte in little endian order" in new UnsignedIntegerScope {

      val bytes = uint64ByteFormatLE(int64)

      parse[Long](bytes,0)(int64ByteReaderLE) === ParseSuccess(int64,8)

    }

    "encode a CompactNumber into byte" in new CompactNumberScope {

      val short12bytes = compactShort12.byteFormat
      val int515bytes = compactInt515.byteFormat
      val long3294967295bytes = compactLong3294967295.byteFormat
      val bigInt14151776774302809990bytes = compactBigInt14151776774302809990.byteFormat

      parse[CompactNumber](short12bytes,0) === ParseSuccess(compactShort12,1)
      parse[CompactNumber](int515bytes,0) === ParseSuccess(compactInt515,3)
      parse[CompactNumber](long3294967295bytes,0) === ParseSuccess(compactLong3294967295,5)
      parse[CompactNumber](bigInt14151776774302809990bytes,0) === ParseSuccess(compactBigInt14151776774302809990,9)

    }

    "encode an Outpoint into byte" in {

      val hash = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069"

      val outpoint = Outpoint(
        hash = hash.hex2bytes,
        12345667L
      )

      val ParseSuccess(out,used) = parse[Outpoint](outpoint.byteFormat,0)

      used === 36
      bytes2hex(out.hash) === hash
      out.index === outpoint.index
    }

    "encode a TransactionInput into byte" in {
      val hash = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069"
      val index = 12345667L
      val scrLen = CompactInt(3)
      val script = "86a73d"
      val seqNum = 9876542L


      val expectedTxIn = TransactionInput(
        previousOutput = Outpoint(
          hash = hash.hex2bytes,
          index
        ),
        scriptLength = scrLen,
        Script(script.hex2bytes),
        seqNum
      )

      val ParseSuccess(txIn,used) = parse[TransactionInput](expectedTxIn.byteFormat,0)

      txIn.sequence === expectedTxIn.sequence
      txIn.scriptLength === expectedTxIn.scriptLength
      txIn.previousOutput.index === expectedTxIn.previousOutput.index

    }

    "encode a TransactionOutput into byte" in {

      val expectedTxOut = TransactionOutput(
        value = 35000,
        pkScriptLength = CompactInt(5),
        pkScript = Script("0c1c1e771a" hex2bytes)
      )

      val ParseSuccess(txOut,used) = parse[TransactionOutput](expectedTxOut.byteFormat,0)

      expectedTxOut.value === txOut.value
      expectedTxOut.pkScriptLength === txOut.pkScriptLength
      bytes2hex(expectedTxOut.pkScript.byteFormat) === bytes2hex(txOut.pkScript.byteFormat)

    }

    "encode a Transaction into byte" in {

      val hash = "86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069"
      val index = 12345667L
      val scrLen = CompactInt(3)
      val script = "86a73d"
      val seqNum = 9876542L

      val expectedTxIn = TransactionInput(
        previousOutput = Outpoint(
          hash = hash.hex2bytes,
          index
        ),
        scriptLength = scrLen,
        Script(script.hex2bytes),
        seqNum
      )
      val expectedTxOut = TransactionOutput(
        value = 35000,
        pkScriptLength = CompactInt(5),
        pkScript = Script("0c1c1e771a" hex2bytes)
      )

      val expectedTx = Transaction(
        version = 1,
        nTxIn = CompactInt(1),
        txIn = Array(expectedTxIn),
        nTxOut = CompactInt(1),
        txOut = Array(expectedTxOut),
        lockTime = 0
      )


      val ParseSuccess(tx,used) = parse[Transaction](expectedTx.byteFormat,0)

      expectedTx.byteFormat.length === used
      expectedTx.version === tx.version
      expectedTx.nTxIn === tx.nTxIn
      expectedTx.nTxOut === tx.nTxOut
      expectedTx.lockTime === tx.lockTime

    }

   "encode Transaction 58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e" in {
     val hex = scala.io.Source.fromFile(getClass.getResource("/58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e.hex").getFile).mkString

     val ParseSuccess(tx,used) = parse[Transaction](hex)

     bytes2hex(tx.byteFormat) === hex
   }

    "encode a BlockHeader into bytes" in {

      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"

      val bal = "0200000066191da95594aeda1a98a19ff054a88a510754e2a4d93e0a00000000000000008485ae79"
      val ParseSuccess(expectedBlockHeader,expectedUsed) = parse[BlockHeader](rawBlockHeader)

      val ParseSuccess(blockHeader,used) = parse[BlockHeader](expectedBlockHeader.byteFormat,0)

      expectedBlockHeader.byteFormat.length === used and used === 80 and used === expectedUsed
      expectedBlockHeader.version === blockHeader.version
      expectedBlockHeader.time === blockHeader.time
      expectedBlockHeader.nBits === blockHeader.nBits
      expectedBlockHeader.nonce === blockHeader.nonce

    }

    "encode blockheader of block 000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9" in {
      val hex = scala.io.Source.fromFile(getClass.getResource("/000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9_header.hex").getFile).mkString

      val ParseSuccess(header,used) = parse[BlockHeader](hex)

      bytes2hex(header.byteFormat) === hex

    }

    "encode a Block into bytes" in {

      val tx = "01000000059277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99010000006b483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7ffffffff2cb46385bbdce98623916455ede6f88b44520df8d6a0faee744b9588533000c3000000006a473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587cffffffff7f5697c119c34c6e3c728c29d69d53b1040c9cfc7a8dcfaf2903463c02431f72000000006b483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039fffffffff9ffceab48b9cf61cc316b58c51fff325f2b70de3be5ff55c887b4e17bc5b3628010000006a47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1ffffffffbbccb3007f7ebef04fd8d3fe9f97601dee5e1f87589e31fc2e03b8ddcd8fd5c6020000006b483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56ffffffff0500c2eb0b000000001976a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac04edf900000000001976a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac00f90295000000001976a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac25bd3e06000000001976a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac1f351200000000001976a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac00000000"
      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"
      val nTx = "01"
      val rawBlock = rawBlockHeader ++ nTx ++ tx

      val ParseSuccess(expectedBlock,expectedUsed) = parse[Block](rawBlock)

      val ParseSuccess(block,used) = parse[Block](expectedBlock.byteFormat,0)

      expectedBlock.byteFormat.length === used and used === expectedUsed
      expectedBlock.numTransaction === block.numTransaction
      expectedBlock.header.nonce === block.header.nonce
      expectedBlock.txs.head.lockTime === block.txs.head.lockTime
      bytes2hex(expectedBlock.byteFormat) === rawBlock

    }

    "encode Block 000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9" in {
      val hex = scala.io.Source.fromFile(getClass.getResource("/000000000000000001f942eb4bfa0aeccb6a14c268f4c72d5fff17270da771b9.hex").getFile).mkString

      val ParseSuccess(block,_) = parse[Block](hex)
      val ParseSuccess(block_parsed,_) = parse[Block](block.byteFormat,0)

      bytes2hex(block_parsed.byteFormat) === hex

    }

  }

}
