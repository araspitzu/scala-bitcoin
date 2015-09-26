import crypto.Hash
import domain.Transaction
import domain.consensus.Script
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
import encoding.Parsing._

class ScriptSpec extends Specification {

  "Script" should {

    "parse a simple script" in {
      val bytes = "0101010293".hex2bytes
      val script = Script(bytes)
      success
    }

    "parse p2pkh pubkeyScript" in {
      val bytes = "76a914e7b369882ebdd4b5907012e3f3e98ab0fbafd01888ac".hex2bytes
      val script = Script(bytes)

      script.toString === "OP_DUP OP_HASH160 e7b369882ebdd4b5907012e3f3e98ab0fbafd018 OP_EQUALVERIFY OP_CHECKSIG "
    }

    "parse a sigScript" in {
      val bytes =  "483045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401210270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3".hex2bytes
      val script = Script(bytes)

      script.toString === "3045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401 0270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3 "
    }

    "extract a pubkey from scripSig" in {

      val scriptBytes = "47304402202b4da291cc39faf8433911988f9f49fc5c995812ca2f94db61468839c228c3e90220628bff3ff32ec95825092fa051cba28558a981fcf59ce184b14f2e215e69106701410414b38f4be3bb9fa0f4f32b74af07152b2f2f630bc02122a491137b6c523e46f18a0d5034418966f93dfc37cc3739ef7b2007213a302b7fba161557f4ad644a1c".hex2bytes

      val script = Script(scriptBytes)

      val hash = Hash.hash160(script.getPubKey)

      bytes2hex(hash) === "mkFQohBpy2HDXrCwyMrYL5RtfrmeiuuPY2"

    }

    "validate a script in tx 58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e" in {
      //val  hex = scala.io.Source.fromFile(getClass.getResource("/58d00055cae1c410cb57462c9d5d56a536284a5abc02a1ac54dd4f79cb731d3e.hex").getFile).mkString

      //val ParseSuccess(tx, used) = parse[Transaction](hex.hex2bytes, 0)

      //get a sigScript
      //val sigScript = tx.txIn.head.signatureScript
      val sigScript = Script("483045022074f35af390c41ef1f5395d11f6041cf55a6d7dab0acdac8ee746c1f2de7a43b3022100b3dc3d916b557d378268a856b8f9a98b9afaf45442f5c9d726fce343de835a58012102c34538fc933799d972f55752d318c0328ca2bacccd5c7482119ea9da2df70a2f".hex2bytes)
      //get a pubkeyScript
//      val pubkeyScript = tx.txOut.head.pkScript
      val pubkeyScript = Script("76a9145e4ff47ceb3a51cdf7ddd80afc4acc5a692dac2d88ac".hex2bytes)

      //run txIn against txOut
      pubkeyScript.verify(sigScript) must beTrue
    }

  }

}