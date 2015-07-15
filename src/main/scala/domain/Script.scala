package domain

import encoding.Writing.ByteWritable


/**
 * Created by andrea on 04/07/15.
 */
case class Script(bytes:Array[Byte]) extends  ByteWritable {

  def length = bytes.length

  override def byteFormat = bytes.toList

}

object Script {


}
