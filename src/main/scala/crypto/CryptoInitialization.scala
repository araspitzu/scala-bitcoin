package crypto

import java.security.Security
import org.bouncycastle.jce.provider.BouncyCastleProvider

/**
  * Ensures the bouncy castle provider is available for the underlying class
  */
trait CryptoInitialization {

  if(!Security.getProviders.exists(_.getName == "BC"))
    Security.addProvider(new BouncyCastleProvider)

}