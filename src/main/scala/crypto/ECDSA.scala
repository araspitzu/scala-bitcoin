package crypto

import java.security.{KeyPair, SecureRandom, KeyPairGenerator}

import crypto.ECSignature.ECSignature
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPublicKeyParameters}
import org.bouncycastle.crypto.signers.ECDSASigner

/**
 * Created by andrea on 13/09/15.
 */
object ECDSA extends CryptoInitialization {

  val ecParams = SECNamedCurves.getByName("secp256k1")
  val curve = new ECDomainParameters(
    ecParams.getCurve,
    ecParams.getG,
    ecParams.getN,
    ecParams.getH
  )

  object ECDSASigner {

    def verify(data:Array[Byte], sig:TransactionSignature, pubKey:Array[Byte]): Boolean =
      verify(data, (sig.r, sig.s), pubKey)


    private def verify(data:Array[Byte], signature: ECSignature, pubKey:Array[Byte]):Boolean = {

      val (r, s) = signature

      val signer = new ECDSASigner
      val params = new ECPublicKeyParameters(curve.getCurve.decodePoint(pubKey), curve)

      signer.init(false, params)
      signer.verifySignature(data, r.bigInteger, s.bigInteger)
    }

  }

}