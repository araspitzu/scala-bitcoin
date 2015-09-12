package consensusSpec

import domain.consensus.{ScriptNumber, Script}
import encoding.CommonParsersImplicits._
import org.specs2.mutable.Specification
/**
 * Created by andrea on 05/09/15.
 */
class ScriptNumberSpec extends Specification {

  "ScriptNumber" should {

    "interpret the following bytes as number" in {
      val bytes = "".hex2bytes
      ScriptNumber(bytes, false) === ScriptNumber(BigInt(0))
    }

    "format the script number as bytes" in {
      ScriptNumber(BigInt(0)).byteFormat === Array.emptyByteArray
    }

  }

}
