import domain.consensus.Script
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
import encoding.Parsing._

class ScriptSpec extends Specification {

  "Script" should {

    "parse p2sh pubkeyScript" in {
      val bytes = "76a914e7b369882ebdd4b5907012e3f3e98ab0fbafd01888ac".hex2bytes

      Script(bytes).toString === "OP_DUP OP_HASH160 e7b369882ebdd4b5907012e3f3e98ab0fbafd018 OP_EQUALVERIFY OP_CHECKSIG "
    }

    "parse a sigScript" in {
      val bytes =  "483045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401210270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3".hex2bytes

      Script(bytes).toString === "3045022045666fd6805ab5264acdc3d2fcbffc27d0482ef1e0d5dcdb958b18db50767f05022100f53d0fd0ce7951f45beaaa835fbcf503a167026a685aed83c54dddceafdd58a401 0270f83fc138312056466b13236680642afcffd493fe1866cc74baddee2cf79ba3 "

    }

  }

}