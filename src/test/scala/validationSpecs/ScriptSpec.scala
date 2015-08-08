package validationSpecs

import org.specs2.mutable.Specification
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


  }

}
