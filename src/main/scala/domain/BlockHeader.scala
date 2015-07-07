package domain

import encoding.Parsing._
import encoding.CommonParsersImplicits._
/**
 * Created by andrea on 02/07/15.
 *
 * The hashes are in internal byte order; the other values are all in little-endian order.
 */
case class BlockHeader(
  version:Long,
  prevHeaderHash:Array[Byte], //length = 32
  merkleRootHash:Array[Byte], //length = 32
  time:Long,
  nBits:Long,
  nonce:Long
)

object BlockHeader {

  implicit val blockHeaderByteReadable = new {} with ByteReadable[BlockHeader] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[BlockHeader] = for {
      (version,used) <- parse[Long](bytes,offset)(uint32ByteReaderLE).withOffset
      (prev_header_hash,used1) <- parseList[Byte](bytes, offset + used, 32).withOffset
      (merkle_root_hash,used2) <- parseList[Byte](bytes, offset + used + used1, 32).withOffset
      (time,used3) <- parse[Long](bytes,offset + used + used1 + used2)(uint32ByteReaderLE).withOffset
      (nBits,used4) <- parse[Long](bytes,offset + used + used1 + used2 + used3)(uint32ByteReaderLE).withOffset
      nonce <- parse[Long](bytes,offset + used + used1 + used2 + used3 + used4)(uint32ByteReaderLE)
    } yield BlockHeader(
      version = version,
      prevHeaderHash = prev_header_hash.toArray,
      merkleRootHash = merkle_root_hash.toArray,
      time = time,
      nBits = nBits,
      nonce = nonce
    )
  }

}