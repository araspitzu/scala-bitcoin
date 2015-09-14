package crypto

import java.security.{KeyPair, SecureRandom, KeyPairGenerator}
import java.security.spec.ECGenParameterSpec

import org.bouncycastle.crypto.signers.ECDSASigner

/**
 * Created by andrea on 13/09/15.
 */
object ECDSA {

  val i = Hash.bcProviderIndex
  val ecGenSpec = new ECGenParameterSpec("secp256k1")
  val keyPairGenerator = KeyPairGenerator.getInstance("ECDSA", "BC")
  keyPairGenerator.initialize(ecGenSpec, new SecureRandom)

  def getKeyPair:KeyPair = keyPairGenerator.generateKeyPair


  object ECDSASigner {

    val signer = new ECDSASigner
  }

}