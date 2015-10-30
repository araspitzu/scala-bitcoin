import crypto.Hash._
import domain.{VersionedChecksummed, Address, Transaction}
import domain.consensus.Script
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._

class ScriptSpec extends Specification {

  val scriptPubKey = "76a91433e81a941e64cda12c6a299ed322ddbdd03f8d0e88ac"

  "Script" should {

    "parse p2pkh scriptPubKey" in {
      val bytes = scriptPubKey.hex2bytes
      val script = Script(bytes)

      script.toString === "OP_DUP OP_HASH160 33e81a941e64cda12c6a299ed322ddbdd03f8d0e OP_EQUALVERIFY OP_CHECKSIG"
    }

    "parse a scriptSig" in {
      val bytes =  "483045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401210270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3".hex2bytes
      val script = Script(bytes)

      script.toString === "3045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401 0270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3"
    }

    "extract the pubkey from scriptPubKey" in {

      val scriptPubKeyBytes = scriptPubKey.hex2bytes
      val script = Script(scriptPubKeyBytes)

      val pubKeyHash = script.getPubKeyHash

      Address(pubKeyHash).toBase58 === "mkFQohBpy2HDXrCwyMrYL5RtfrmeiuuPY2"

    }

    "extract the pubkey from scriptSig" in {

      val scriptBytes = "47304402202b4da291cc39faf8433911988f9f49fc5c995812ca2f94db61468839c228c3e90220628bff3ff32ec95825092fa051cba28558a981fcf59ce184b14f2e215e69106701410414b38f4be3bb9fa0f4f32b74af07152b2f2f630bc02122a491137b6c523e46f18a0d5034418966f93dfc37cc3739ef7b2007213a302b7fba161557f4ad644a1c".hex2bytes
      val script = Script(scriptBytes)

      val hash = hash160(script.getPubKey)

      Address(hash).toBase58 === "mkFQohBpy2HDXrCwyMrYL5RtfrmeiuuPY2"

    }


  }

}