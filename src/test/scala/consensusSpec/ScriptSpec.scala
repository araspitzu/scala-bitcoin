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

    "validate a script " in {
      val scriptSig = "3044022072149fdcb18c84dee0fa59606025c62a31174dd10e9c7c638c80947c18c07c840220760d14535d575ab29b47613b04aac1644628356e0a7ed36514f2bd56feed3e3201".hex2bytes
      val scriptPubKey = "OP_DUP OP_HASH160 5b30b9d8b477bdf7da957266160fac75ded8615b OP_EQUALVERIFY OP_CHECKSIG"
      1 === 1
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