package crypto

import java.security.{KeyPair, SecureRandom, KeyPairGenerator}
import java.security.spec.ECGenParameterSpec

import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.signers.ECDSASigner
import org.bouncycastle.jce.ECNamedCurveTable

/**
 * Created by andrea on 13/09/15.
 */
object ECDSA extends CryptoInitialization {

  val ecGenSpec = new ECGenParameterSpec("secp256k1")
  val keyPairGenerator = KeyPairGenerator.getInstance("ECDSA", "BC")
  val curveParams = ECNamedCurveTable.getParameterSpec("secp256k1")


  keyPairGenerator.initialize(ecGenSpec, new SecureRandom)

  def getKeyPair:KeyPair = keyPairGenerator.generateKeyPair


  object ECDSASigner {

    val signer = new ECDSASigner
  }

}