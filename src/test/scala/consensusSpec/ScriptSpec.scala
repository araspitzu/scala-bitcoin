import domain.consensus.Script
import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
import encoding.Parsing._

class ScriptSpec extends Specification {

  "Script" should {

    "parse p2sh script" in {
      val bytes = "76a914e7b369882ebdd4b5907012e3f3e98ab0fbafd01888ac".hex2bytes

      val ParseSuccess(script, used) = parse[Script](bytes, 0)

      script.toString === "OP_DUP OP_HASH160 e7b369882ebdd4b5907012e3f3e98ab0fbafd018 OP_EQUALVERIFY OP_CHECKSIG "
    }

  }

}