package domain

import encoding.Writing.ByteWritable


/**
 * Created by andrea on 04/07/15.
 */
case class Script(bytes:Array[Byte]) extends  ByteWritable {
 // require(bytes.length == 12,s"Script is ${bytes.length} bytes long")

  def length = bytes.length
  override def byteFormat = bytes

  def execute = {

  }

  private def execute(script:Array[Byte],stack:Array[Array[Byte]]) = {

  }


}

object Script {

  object OP_CODES {

    val OP_TRUE = 0x51
    val OP_FALSE = 0x00

    val OP_0 = 0x00
    val OP_1 = 0x51
    val OP_2 = 0x52
    val OP_3 = 0x53
    val OP_4 = 0x54
    val OP_5 = 0x55
    val OP_6 = 0x56
    val OP_7 = 0x57
    val OP_8 = 0x58
    val OP_9 = 0x59
    val OP_10 = 0x5a
    val OP_11 = 0x5b
    val OP_12 = 0x5c
    val OP_13 = 0x5d
    val OP_14 = 0x5e
    val OP_15 = 0x5f
    val OP_16 = 0x60

    val OP_NOP = 0x61
    val OP_IF = 0x63
    val OP_NOTIF = 0x64
    val OP_ELSE = 0x67
    val OP_ENDIF = 0x67
    val OP_VERIFY = 0x69
    val OP_RETURN = 0x6a


    // N/A = 0x01 - 0x4b
    val OP_PUSHDATA1 = 0x4c
    val OP_PUSHDATA2 = 0x4d
    val OP_PUSHDATA4 = 0x4e
    val OP_1NEGATE = 0x4f



  }

}
