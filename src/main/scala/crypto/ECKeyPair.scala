package crypto

import java.security.spec.ECGenParameterSpec
import java.security._

import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.params.{ECKeyGenerationParameters, ECDomainParameters, ECPublicKeyParameters, ECPrivateKeyParameters}
import org.bouncycastle.crypto.signers.ECDSASigner
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.math.ec.ECPoint


/**
 * Created by andrea on 24/09/15.
 */
case class ECKeyPair(priv: BigInt, pub: ECPoint) extends CryptoInitialization {

}

object ECKeyPair extends CryptoInitialization {

  val CURVE_NAME = "secp256k1"
  val ecSpec = ECNamedCurveTable.getParameterSpec(CURVE_NAME)
  val domainParam = new ECDomainParameters(
    ecSpec.getCurve,
    ecSpec.getG,
    ecSpec.getN,
    ecSpec.getH
  )

  def apply():ECKeyPair = {
    val random = new SecureRandom
    val keyParGenerator = new ECKeyPairGenerator
    val keyGenerationParams = new ECKeyGenerationParameters(domainParam, random)
    keyParGenerator.init(keyGenerationParams)

    val keyPair = keyParGenerator.generateKeyPair
    val privateKey =  keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val publicKey = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters]

    ECKeyPair(
      privateKey.getD,
      publicKey.getQ
    )
//    val paramSpec = new ECGenParameterSpec(CURVE_NAME)
//
//    val keyGenerator = KeyPairGenerator.getInstance("ECDSA", "BC")
//    keyGenerator.initialize(paramSpec, random)
//
//    val keyPair = keyGenerator.generateKeyPair
//    println(s"keyPair.getPrivate ${keyPair.getPrivate}")
//    println(s"keyPair.getPublic ${keyPair.getPublic}")
//
//    val privKeySpec = keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
//    val pubKeySpec = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters]
//
//    ECKeyPair(
//      priv = privKeySpec.getD,
//      pub =  pubKeySpec.getQ
//    )
  }



}
