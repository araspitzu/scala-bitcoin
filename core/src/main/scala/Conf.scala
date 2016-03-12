import com.typesafe.config.ConfigFactory

/**
 * Created by andrea on 13/06/15.
 */
package object Conf {

  private val config = ConfigFactory.load

  object TxConfig {
    val VERSION = config.getInt("core.tx.VERSION")
    require(VERSION >= 0 && VERSION < 256)
  }

  object ScriptConfig {
    val MAX_SIZE = config.getInt("core.script.MAX_SIZE")
    val MAX_SCRIPT_ELEMENT_SIZE = config.getInt("core.script.MAX_SCRIPT_ELEMENT_SIZE")
  }



}
