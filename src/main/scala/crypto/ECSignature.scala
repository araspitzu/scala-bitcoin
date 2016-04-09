package crypto

import java.io.ByteArrayOutputStream
import java.math.BigInteger

import org.bouncycastle.asn1.{DERInteger, DERSequence, DERSequenceGenerator, ASN1InputStream}

/**
 * Created by andrea on 14/09/15.
 */
object ECSignature {

  type ECSignature = (BigInteger, BigInteger)

  def toByteDER(r: BigInteger, s: BigInteger):Array[Byte] = {
    val bos = new ByteArrayOutputStream(72)
    val seq = new DERSequenceGenerator(bos)
    seq.addObject(new ASN1InputStream(r.toByteArray).readObject)
    seq.addObject(new ASN1InputStream(s.toByteArray).readObject)
    seq.close
    bos.toByteArray
  }

  def decodeFromDER(bytes: Array[Byte]):ECSignature = {
    val derObject = new ASN1InputStream(bytes).readObject
    val derSequence = derObject.asInstanceOf[DERSequence]

    val r = derSequence.getObjectAt(0).asInstanceOf[DERInteger]
    val s = derSequence.getObjectAt(1).asInstanceOf[DERInteger]

    (r.getPositiveValue, s.getPositiveValue)
  }

}
