package domain

import encoding.Parsing._
import domain.CompactNumber._

/**
 * Created by andrea on 02/07/15.
 */
case class Block(
  header:BlockHeader,
  nTx:CompactNumber,
  txs:Array[Transaction]
)

object Block {

  implicit val blockByteReadable = new {} with ByteReadable[Block] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Block] = for {
      header <- parse[BlockHeader](bytes,offset)
      nTx <- parse[CompactNumber](bytes,offset + 80)
      txs <- parseList[Transaction](bytes,offset + 80 + nTx.originalSize , nTx.intValue)
    } yield Block(
      header = header,
      nTx = nTx,
      txs = txs.toArray
    )
  }

}

