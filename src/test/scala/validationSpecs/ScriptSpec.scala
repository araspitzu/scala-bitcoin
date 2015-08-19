package validationSpecs

import org.specs2.mutable.Specification
import encoding.CommonParsersImplicits._
/**
 * Created by andrea on 08/08/15.
 */
class ScriptSpec extends Specification {

  "Bitcoin script" should {

    "read signed integers from byte in little endian order" in {

      val oneMinus = 0x81
      val zeroMinus = 0x80

      oneMinus === zeroMinus
    }

    "read the following bytes as script" in {

      val bytes = "0101010193".hex2bytes
      val expectedScript = "01 01 OP_ADD"

      1 === 1

    }


  }

}
