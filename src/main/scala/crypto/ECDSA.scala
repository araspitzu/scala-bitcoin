package crypto

import crypto.ECSignature.ECSignature
import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPublicKeyParameters}
import org.bouncycastle.crypto.signers.ECDSASigner

/**
 * Created by andrea on 13/09/15.
 */
object ECDSA extends CryptoInitialization {

  val ecParams = SECNamedCurves.getByName("secp256k1")
  val halfCurveOrder = ecParams.getN().shiftRight(1)
  val curve = new ECDomainParameters(
    ecParams.getCurve,
    ecParams.getG,
    ecParams.getN,
    ecParams.getH
  )

  object ECDSASigner {

    def verify(data:Array[Byte], sig:TransactionSignature, pubKey:Array[Byte]): Boolean =
      verify(data, sig.ecSig, pubKey)


    private def verify(data:Array[Byte], signature: ECSignature, pubKey:Array[Byte]):Boolean = {
      //TODO add more checks

      val (r, s) = signature

      val signer = new ECDSASigner
      val params = new ECPublicKeyParameters(curve.getCurve.decodePoint(pubKey), curve)

      signer.init(false, params)
      signer.verifySignature(data, r, s)
    }

    /**
      *  See https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
      * @param sig
      * @return
      */
    def isCanonical(sig:ECSignature):Boolean = {
      val (_, s) = sig
      s.compareTo(halfCurveOrder) <= 0
    }

  }

}