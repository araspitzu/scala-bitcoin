package crypto

import java.security.{MessageDigest, Security}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.crypto.digests.RIPEMD160Digest
/**
 * Created by andrea on 12/09/15.
 */
object Hash {

  val bcJceProvider = new BouncyCastleProvider
  val bcProviderIndex = Security.addProvider(bcJceProvider)

  private val sha256Digest = MessageDigest.getInstance("SHA256")
  private val sha1Digest = MessageDigest.getInstance("SHA1")

  implicit def sha1(bytes: Array[Byte]): Array[Byte] = sha1Digest.digest(bytes)

  implicit def sha256(bytes: Array[Byte]):Array[Byte] = sha256Digest.digest(bytes)

  implicit def ripemd160(bytes: Array[Byte]):Array[Byte] = {
    val ripemd160Digest = new RIPEMD160Digest
    ripemd160Digest.update(bytes, 0, bytes.length)
    val result = Array.fill[Byte](20){0xff.toByte}
    ripemd160Digest.doFinal(result, 0)
    result
  }

  implicit def hash256(bytes: Array[Byte]): Array[Byte] = sha256(sha256(bytes))

  implicit def hash160(bytes: Array[Byte]): Array[Byte] = ripemd160(sha256(bytes))


}
