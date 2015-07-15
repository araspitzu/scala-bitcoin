package domain

import encoding.Parsing._
import domain.CompactNumber._
import encoding.Writing.ByteWritable

/**
 * Created by andrea on 02/07/15.
 */
case class Block(
  header:BlockHeader,
  nTx:CompactNumber,
  txs:List[Transaction]
) extends ByteWritable {

  def byteFormat =
    header.byteFormat ++
    nTx.byteFormat ++
    txs.foldRight[List[Byte]](Nil)( (tx,acc) => tx.byteFormat ++ acc )

}

object Block {

  implicit val blockByteReadable = new {} with ByteReadable[Block] {
    override def read(bytes: Array[Byte], offset: Int): ParseResult[Block] = for {
      header <- parse[BlockHeader](bytes,offset)
      nTx <- parse[CompactNumber](bytes,offset + 80)
      (coinbase,used) <- parse[Transaction](bytes,offset + 80 + nTx.originalSize).withOffset
      if(coinbase.isCoinbase)
      txs <- parseList[Transaction](bytes,offset + 80 + nTx.originalSize + used, nTx.intValue - 1 )
    } yield Block(
      header = header,
      nTx = nTx,
      txs = coinbase :: txs
    )
  }

}

