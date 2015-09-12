package crypto

import java.security.{MessageDigest, Security}
import org.bouncycastle.jce.provider.BouncyCastleProvider
/**
 * Created by andrea on 12/09/15.
 */
object Hash {

  val bcJceProvider = new BouncyCastleProvider
  val bcProviderIndex = Security.addProvider(bcJceProvider)

  val sha256Digest = MessageDigest.getInstance("SHA256")

  def sha256(bytes: Array[Byte]) = sha256Digest.digest(bytes)
}
