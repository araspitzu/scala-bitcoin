import com.typesafe.config.ConfigFactory

/**
 * Created by andrea on 13/06/15.
 */
object Conf {

  private val config = ConfigFactory.load

  object TxConfig {
    val max_script_size = config.getInt("core.tx.SCRIPT_MAX_SIZE")
  }

}
