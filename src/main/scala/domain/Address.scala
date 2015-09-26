package domain

/**
 * Created by andrea on 20/09/15.
 */
case class Address(bytes: Array[Byte]) extends VersionedChecksummed {
  require(bytes.length == 20, "Address must be exactly 20 long, see RIPEMD160")

  val version = 111


}
