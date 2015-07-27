package spikeTest

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
  val uint32bytesBE = "c46535ff"
  val uint32bytesLE = "ff3565c4"

  val uint64bytes = "c46535ff34f13f86"
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
  val compactBigInt14151776774302809990bytes = "ff"+uint64bytes

}

class ParsingSpec extends Specification {

  val rawTx = "01000000059277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99010000006b483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7ffffffff2cb46385bbdce98623916455ede6f88b44520df8d6a0faee744b9588533000c3000000006a473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587cffffffff7f5697c119c34c6e3c728c29d69d53b1040c9cfc7a8dcfaf2903463c02431f72000000006b483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039fffffffff9ffceab48b9cf61cc316b58c51fff325f2b70de3be5ff55c887b4e17bc5b3628010000006a47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1ffffffffbbccb3007f7ebef04fd8d3fe9f97601dee5e1f87589e31fc2e03b8ddcd8fd5c6020000006b483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56ffffffff0500c2eb0b000000001976a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac04edf900000000001976a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac00f90295000000001976a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac25bd3e06000000001976a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac1f351200000000001976a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac00000000"
  val rawSignedTxData =
    """
      |{
      |        "txid": "8f70ec02b1fda4ff4e6d4aa4dba2579f967a8db68b4212606db1024f88134024",
      |        "version": 1,
      |        "locktime": 0,
      |        "vin": [
      |            {
      |                "txid": "999b3cdfd3b4719b0a4610b7e8b8ab6d8f15a98e6003248251fbd7d45fe67792",
      |                "vout": 1,
      |                "scriptSig": {
      |                    "asm": "3045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba3401 038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7",
      |                    "hex": "483045022100e177d5ffe1f50bcba7c8bc4821917b2e4172e5bb911e98811c84cf662dcab0aa02207a9eb299fd5311b16d4a2f8588f3fb63912a2c1d1b5a1f69b0e65ebd319aba340121038f88a5761b78c0a51e530d3399998544c70eabcfea9da6a289ee89511f5e80a7"
      |                },
      |                "sequence": 4294967295
      |            },
      |            {
      |                "txid": "c300305388954b74eefaa0d6f80d52448bf8e6ed5564912386e9dcbb8563b42c",
      |                "vout": 0,
      |                "scriptSig": {
      |                    "asm": "3044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b01 021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587c",
      |                    "hex": "473044022072849eb4402bc6c0bb37c3cf4f0cbbbe9ea46ab6153e3ca5e3de41c017ec736502200bf555f046a65df186ed6b144282f6a803bc3f41cd942e23fd45de300584c17b0121021046bcf28f3d4487386bfb4238931eda4a1ce3b526cb371fdd89ed0eb054587c"
      |                },
      |                "sequence": 4294967295
      |            },
      |            {
      |                "txid": "721f43023c460329afcf8d7afc9c0c04b1539dd6298c723c6e4cc319c197567f",
      |                "vout": 0,
      |                "scriptSig": {
      |                    "asm": "3045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c01 03fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039f",
      |                    "hex": "483045022100cad72c9298d080af79dd64c5b6b179a3596489ca58558566d212f33e23d4eec502205fd6ca03a642439b3d60e75c3d6d78d4299a86dcca830853e04cdd9ec665ac5c012103fbbbcd7d9fa8bfe5ee7c48c68c1712719ebf09f7ee349a8611a4d69a5258039f"
      |                },
      |                "sequence": 4294967295
      |            },
      |            {
      |                "txid": "28365bbc174e7b885cf55fbee30db7f225f3ff518cb516c31cf69c8bb4eafc9f",
      |                "vout": 1,
      |                "scriptSig": {
      |                    "asm": "304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a01 03d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1",
      |                    "hex": "47304402206d7ec1fe6e429941c6622f3513bbef93ff35797d5f52bbbbd509b4d46df7036f02207eb9e20edab4a4eb543c4b483d126c674c41acb5595ccb0ebc5f687c3a5fbe7a012103d88141e90dd56e207b3a122c3fff96353dbcec9fa34ef6c7407a785e13e172f1"
      |                },
      |                "sequence": 4294967295
      |            },
      |            {
      |                "txid": "c6d58fcdddb8032efc319e58871f5eee1d60979ffed3d84ff0be7e7f00b3ccbb",
      |                "vout": 2,
      |                "scriptSig": {
      |                    "asm": "3045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f001 033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56",
      |                    "hex": "483045022100e65e2e2d72a344ef64e360e3f6121bcb950bc2c1a9603df90137aa183622ee7c02201b191319eff2100b20dc6ef7fd3d4c85fd4d04d828bce3ad8534e1ef3a2f74f00121033f646f0990d08c6369965402222b2c3cbfb9f41759ce1030c14255dbc4c9ff56"
      |                },
      |                "sequence": 4294967295
      |            }
      |        ],
      |        "vout": [
      |            {
      |                "value": 2,
      |                "n": 0,
      |                "scriptPubKey": {
      |                    "asm": "OP_DUP OP_HASH160 185cce1fe41c0fb47265a776472fe549fdfd631e OP_EQUALVERIFY OP_CHECKSIG",
      |                    "hex": "76a914185cce1fe41c0fb47265a776472fe549fdfd631e88ac",
      |                    "reqSigs": 1,
      |                    "type": "pubkeyhash",
      |                    "addresses": [
      |                        "13DpPpb54niim6RAot5ZT987VrMybHHYp6"
      |                    ]
      |                }
      |            },
      |            {
      |                "value": 0.1637914,
      |                "n": 1,
      |                "scriptPubKey": {
      |                    "asm": "OP_DUP OP_HASH160 3044b8c793fb25f2f5f36f129473400459aefaaf OP_EQUALVERIFY OP_CHECKSIG",
      |                    "hex": "76a9143044b8c793fb25f2f5f36f129473400459aefaaf88ac",
      |                    "reqSigs": 1,
      |                    "type": "pubkeyhash",
      |                    "addresses": [
      |                        "15QDm5kYFfe7nrhN2aVsJt84PWr5VrDxwX"
      |                    ]
      |                }
      |            },
      |            {
      |                "value": 25,
      |                "n": 2,
      |                "scriptPubKey": {
      |                    "asm": "OP_DUP OP_HASH160 31fa40cfa4effe923c2aaf2acb88a347440faac1 OP_EQUALVERIFY OP_CHECKSIG",
      |                    "hex": "76a91431fa40cfa4effe923c2aaf2acb88a347440faac188ac",
      |                    "reqSigs": 1,
      |                    "type": "pubkeyhash",
      |                    "addresses": [
      |                        "15ZFuJ6ExNiCad5nXv8yjKAKeLthQLLPkJ"
      |                    ]
      |                }
      |            },
      |            {
      |                "value": 1.04774949,
      |                "n": 3,
      |                "scriptPubKey": {
      |                    "asm": "OP_DUP OP_HASH160 b4e43f5f7ba1f3897121609dd198172d9203d2c7 OP_EQUALVERIFY OP_CHECKSIG",
      |                    "hex": "76a914b4e43f5f7ba1f3897121609dd198172d9203d2c788ac",
      |                    "reqSigs": 1,
      |                    "type": "pubkeyhash",
      |                    "addresses": [
      |                        "1HVU4SwoGmgGUqwwW1MP8y1c78MoUako3B"
      |                    ]
      |                }
      |            },
      |            {
      |                "value": 0.01193247,
      |                "n": 4,
      |                "scriptPubKey": {
      |                    "asm": "OP_DUP OP_HASH160 9b58d4b372b7aa8b5076b49df7f15441fb789655 OP_EQUALVERIFY OP_CHECKSIG",
      |                    "hex": "76a9149b58d4b372b7aa8b5076b49df7f15441fb78965588ac",
      |                    "reqSigs": 1,
      |                    "type": "pubkeyhash",
      |                    "addresses": [
      |                        "1FAQ9ozpYXtNqWD9d811Hd8o8UomnYCk5E"
      |                    ]
      |                }
      |            }
      |        ]
      |    }
    """.stripMargin


  "The transacion serializer" should {

    "parse an uint8 as Short" in new UnsignedIntegerScope{

      parse[Short](uint8bytes.hex2bytes,0) === ParseSuccess(uint8,1)

    }

    "parse an uint16 as Int" in new UnsignedIntegerScope{

      parse[Int](uint16bytes.hex2bytes,0) === ParseSuccess(uint16,2)

    }

    "parse an uint32 in little endian order as Long" in new UnsignedIntegerScope {

      parse[Long](uint32bytesLE.hex2bytes,0)(uint32ByteReaderLE) === ParseSuccess(uint32,4)

    }

    "parse an uint64 as BigInt" in new UnsignedIntegerScope {

      parse[BigInt](uint64bytes.hex2bytes,0) === ParseSuccess(BigInt(uint64decimal,10),8)

    }

    "parse another uint64 as BigInt" in new UnsignedIntegerScope {

      val rawUint64bytes = "000000012a05caf0"
      val rawUint64decimal = "4999990000"

      parse[BigInt](rawUint64bytes.hex2bytes,0) === ParseSuccess(BigInt(rawUint64decimal,10),8)

    }

    "parse an int64 as Long" in new UnsignedIntegerScope {

      parse[Long](int64bytes.hex2bytes,0)(int64ByteReader) === ParseSuccess(int64,8)

    }

    "parse a CompactNumber using the proper type" in new CompactNumberScope{

      parse[CompactNumber](compactShort12bytes.hex2bytes,0) === ParseSuccess(compactShort12,1)
      parse[CompactNumber](compactInt515bytes.hex2bytes,0) === ParseSuccess(compactInt515,3)
      parse[CompactNumber](compactLong3294967295bytes.hex2bytes,0) === ParseSuccess(compactLong3294967295,5)
      parse[CompactNumber](compactBigInt14151776774302809990bytes.hex2bytes,0) === ParseSuccess(compactBigInt14151776774302809990,9)

    }

    "parse an Outpoint" in new CompactNumberScope {
      import domain.TransactionInput._

      val expectedIndex = uint32
      val hash = "9277e65fd4d7fb51822403608ea9158f6dabb8e8b710460a9b71b4d3df3c9b99"

      val rawOutpoint:Array[Byte] = (hash ++ uint32bytesLE).hex2bytes

      val ris = parse[Outpoint](rawOutpoint,0)

      val outpoint:Outpoint = ris.get._1

      ris.get._2 === 36

      outpoint.hash.length === hash.length / 2
      outpoint.index === expectedIndex

    }

    "parse a TransactionInput" in new CompactNumberScope {

      val rawOutpoint = s"86a73d7aad94571e040ae307e866b53605255baf85b9ffc874872b4c4586b069$uint32bytesLE"
      val script = "b2c3ffb2c3ffb2c3ffb2c3ff"
      val scrLen = compactShort12bytes

      val hexTxIn = rawOutpoint ++ scrLen ++ script ++ uint32bytesLE
      val rawTxIn = hexTxIn.hex2bytes

      val expectedLength = 36 + 1 + 12 + 4

      val ris =  parse[TransactionInput](rawTxIn,0)

      val txIn = ris.get._1
      val txInLength = ris.get._2

      txIn.previousOutput.index === uint32
      txIn.sequence === uint32
      txIn.scriptLength === compactShort12

      txInLength === expectedLength

    }

    "parse a raw TxIn" in {
      val rawTxIn = "6dbddb085b1d8af75184f0bc01fad58d1266e9b63b50881990e4b40d6aee3629000000008b483045022100f3581e1972ae8ac7c7367a7a253bc1135223adb9a468bb3a59233f45bc578380022059af01ca17d00e41837a1d58e97aa31bae584edec28d35bd96923690913bae9a0141049c02bfc97ef236ce6d8fe5d94013c721e915982acd2b12b65d9b7d59e20a842005f8fc4e02532e873d37b96f09d6d4511ada8f14042f46614a4c70c0f14beff5ffffffff"

      val ris = parse[TransactionInput](rawTxIn.hex2bytes,0)

      val txIn = ris.get._1

      bytes2hex(txIn.scriptLength.byteFormat.toArray) === "8b"

      bytes2hex(txIn.byteFormat.toArray) === rawTxIn

    }

    "parse a TransactionOutput" in new CompactNumberScope {

      val rawTxOut = int64bytes ++ compactShort12bytes ++ "c46535ff34f13f863f863f86"

      val res = parse[TransactionOutput](rawTxOut.hex2bytes, 0)

      val txOut = res.get._1

      txOut.value === int64
      txOut.pkScriptLength === compactShort12
      txOut.pkScript.length === 12

    }

    "parse a Transaction" in new CompactNumberScope {

      val res = parse[Transaction](rawTx.hex2bytes,0)

      val tx:Transaction = res.get._1
      val txLength = res.get._2

      tx.version === 1L
      tx.nTxIn === CompactInt(5)
      tx.txIn.map(_.sequence) forall (_ == 4294967295L)
      tx.nTxOut === CompactInt(5)
      tx.txOut(0).value === 200000000L
      tx.lockTime === 0L

    }

    "parse a BlockHeader" in {

      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"

      val res = parse[BlockHeader](rawBlockHeader.hex2bytes,0)

      val header = res.get._1
      val length = res.get._2

      header.version === 2
      header.time === 1415239972 //Unix timestamp
      length === 80
    }

    "parse a Block" in {
      val rawBlockHeader = "02000000b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c00000000000000009d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab3147124d95a5430c31b18fe9f0864"
      val nTx = "03"
      val tx = rawTx

      val rawBlock = rawBlockHeader ++ nTx ++ tx ++ tx ++ tx

      val res = parse[Block](rawBlock.hex2bytes,0)

      val block = res.get._1
      val blockSize = res.get._2

      block.header.version === 2
      block.header.time === 1415239972L
      block.nTx === CompactInt(3)
      block.txs.length === 3
      block.txs forall (_.version == 1)
      block.txs forall (_.nTxOut === CompactInt(5))
      blockSize === rawBlock.length / 2

    }
  }

}
