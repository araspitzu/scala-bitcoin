package crypto

import java.security.Security
import org.bouncycastle.jce.provider.BouncyCastleProvider

trait CryptoInitialization {

  if(!Security.getProviders.exists(_.getName == "BC"))
    Security.addProvider(new BouncyCastleProvider)


}