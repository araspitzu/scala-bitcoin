package domain

import encoding.Writing.ByteWritable


/**
 * Created by andrea on 04/07/15.
 */
case class Script(bytes:Array[Byte]) extends ByteWritable {
 // require(bytes.length == 12,s"Script is ${bytes.length} bytes long")

  import domain.Script.OP_CODES._

  val FALSE:Int = 0
  val TRUE:Int = 1

  def length = bytes.length
  override def byteFormat = bytes

  def execute:Int = execute(bytes,List.empty)

  private def execute(script:Array[Byte],stack:List[List[Byte]]):Int = script.headOption.map { opcode =>
    opcode match {
      //push TRUE on the stack
      case OP_TRUE => execute(script.tail, (OP_TRUE::Nil)::stack  )
      //push FALSE on the stack
      case OP_TRUE => execute(script.tail, (OP_FALSE::Nil)::stack  )
      //given OP_N,pushes N on the stack
      case OP_0 => execute(script.tail, (0.toByte::Nil)::stack  )
      case OP_1 => execute(script.tail, (1.toByte::Nil)::stack  )
      //does nothing
      case OP_NOP => execute(script.tail,stack)
      case _ => 2
    }
    //if the script is finished or empty script was given, evaluate the stack as number
  } getOrElse {
    stack.headOption.map { bytes:List[Byte] =>
      1
      //if the stack is empty
    }.getOrElse(FALSE)
  }

}

object Script {

  object OP_CODES extends Enumeration {

    val OP_TRUE = 0x51.toByte
    val OP_FALSE = 0x00.toByte

    val OP_0 = 0x00.toByte
    val OP_1 = 0x51.toByte
    val OP_2 = 0x52.toByte
    val OP_3 = 0x53.toByte
    val OP_4 = 0x54.toByte
    val OP_5 = 0x55.toByte
    val OP_6 = 0x56.toByte
    val OP_7 = 0x57.toByte
    val OP_8 = 0x58.toByte
    val OP_9 = 0x59.toByte
    val OP_10 = 0x5a.toByte
    val OP_11 = 0x5b.toByte
    val OP_12 = 0x5c.toByte
    val OP_13 = 0x5d.toByte
    val OP_14 = 0x5e.toByte
    val OP_15 = 0x5f.toByte
    val OP_16 = 0x60.toByte

    val OP_NOP = 0x61.toByte
    val OP_IF = 0x63.toByte
    val OP_NOTIF = 0x64.toByte
    val OP_ELSE = 0x67.toByte
    val OP_ENDIF = 0x67.toByte
    val OP_VERIFY = 0x69.toByte
    val OP_RETURN = 0x6a.toByte

    //TODO N/A = 0x01 - 0x4b
    val OP_PUSHDATA1 = 0x4c.toByte
    val OP_PUSHDATA2 = 0x4d.toByte
    val OP_PUSHDATA4 = 0x4e.toByte
    val OP_1NEGATE = 0x4f.toByte

    val OP_TOALTSTACK = 0x6b.toByte
    val OP_FROMALTSTACK = 0x6c.toByte
    val OP_IFDUP = 0x73.toByte
    val OP_DEPTH = 0x74.toByte
    val OP_DROP = 0x75.toByte
    val OP_DUP = 0x76.toByte
    val OP_NIP = 0x77.toByte
    val OP_OVER = 0x78.toByte
    val OP_PICK = 0x79.toByte
    val OP_ROLL = 0x7a.toByte
    val OP_ROT = 0x7b.toByte
    val OP_SWAP = 0x7c.toByte
    val OP_TUCK = 0x7d.toByte
    val OP_2DROP = 0x6d.toByte
    val OP_2DUP = 0x6e.toByte
    val OP_3DUP = 0x6f.toByte
    val OP_2OVER = 0x70.toByte
    val OP_2ROT = 0x71.toByte
    val OP_2SWAP = 0x72.toByte

    //OP_CAT
    //OP_SUBSTR
    //OP_LEFT
    //OP_RIGHT
    val OP_SIZE = 0x82.toByte

    //OP_INVERT
    //OP_AND
    //OP_OR
    //OP_XOR
    val OP_EQUAL = 0x87.toByte
    val OP_EQUALVERIFY = 0x88.toByte

    val OP_1ADD = 0x8b.toByte
    val OP_1SUB = 0x8c.toByte
    //OP_2MUL
    //OP_2DIV
    val OP_NEGATE = 0x8f.toByte
    val OP_ABS = 0x90.toByte
    val OP_NOT = 0x91.toByte
    val OP_NOTEQUAL = 0x92.toByte
    val OP_ADD = 0x93.toByte
    val OP_SUB = 0x94.toByte
    //OP_MUL
    //OP_DIV
    //OP_MOD
    //OP_LSHIFT
    //OP_RSHIFT
    val OP_BOOLAND = 0x9a.toByte
    val OP_BOOLOR = 0x9b.toByte
    val OP_NUMEQUAL = 0x9c.toByte
    val OP_NUMEQUALVERIFY = 0x9d.toByte
    val OP_NUMNOTEQUAL = 0x9e.toByte
    val OP_LESSTHAN = 0x9f.toByte
    val OP_GREATERTHAN = 0xa0.toByte
    val OP_LESSTHANOREQUAL = 0xa1.toByte
    val OP_GREATERTHANOREQUAL = 0xa2.toByte
    val OP_MIN = 0xa3.toByte
    val OP_MAX = 0xa4.toByte
    val OP_WITHIN = 0xa5.toByte

    val OP_RIPEMD160 = 0xa6.toByte
    val OP_SHA1 = 0xa7.toByte
    val OP_SHA256 = 0xa8.toByte
    val OP_HASH160 = 0xa9.toByte
    val OP_HASH256 = 0xaa.toByte
    val OP_CODESEPARATOR = 0xab.toByte
    val OP_CHECKSIG = 0xac.toByte
    val OP_CHECKSIGVERIFY = 0xad.toByte
    val OP_CHECKMULTISIG = 0xae.toByte
    val OP_CHECKMULTISIGVERIFY = 0xaf.toByte
    val OP_PUBKEYHASH = 0xfd.toByte
    val OP_PUBKEY = 0xfe.toByte
    val OP_INVALIDOPCODE = 0xff.toByte

  }

}
