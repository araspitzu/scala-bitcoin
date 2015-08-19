package domain

import domain.Script.OP_CODES
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

  override def toString = bytes.foldLeft("") { (acc,op) => acc + OP_CODES(op).toString }

  def execute:Int = execute(bytes,List.empty)

  private def execute(script:Array[Byte],stack:List[List[Byte]]):Int = script.headOption.map { opcode =>
    OP_CODES(opcode) match {
      //push TRUE on the stack
      case OP_TRUE => execute(script.tail, (OP_TRUE.toByte::Nil)::stack  )
      //push FALSE on the stack
      case OP_FALSE => execute(script.tail, (OP_FALSE.toByte::Nil)::stack  )
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



 case object OP_CODES extends Enumeration {

    implicit def toByte(opcode:Script.OP_CODES.Value):Byte = opcode.id.toByte

    val OP_TRUE = Value(0x51, "OP_TRUE")
    val OP_FALSE = Value(0x00, "OP_FALSE")

    val OP_0 = Value(0x00, "OP_0")
    val OP_1 = Value(0x51, "OP_1")
    val OP_2 = Value(0x52, "OP_2")
    val OP_3 = Value(0x53, "OP_3")
    val OP_4 = Value(0x54, "OP_4")
    val OP_5 = Value(0x55, "OP_5")
    val OP_6 = Value(0x56, "OP_6")
    val OP_7 = Value(0x57, "OP_7")
    val OP_8 = Value(0x58, "OP_8")
    val OP_9 = Value(0x59, "OP_9")
    val OP_10 = Value(0x5a, "OP_10")
    val OP_11 = Value(0x5b, "OP_11")
    val OP_12 = Value(0x5c, "OP_12")
    val OP_13 = Value(0x5d, "OP_13")
    val OP_14 = Value(0x5e, "OP_14")
    val OP_15 = Value(0x5f, "OP_15")
    val OP_16 = Value(0x60, "OP_16")

    val OP_NOP = Value(0x61, "OP_NOP")
    val OP_IF = Value(0x63, "OP_IF")
    val OP_NOTIF = Value(0x64, "OP_NOTIF")
    val OP_ELSE = Value(0x67, "OP_ELSE")
    val OP_ENDIF = Value(0x68, "OP_ENDIF")
    val OP_VERIFY = Value(0x69, "OP_VERIFY")
    val OP_RETURN = Value(0x6a, "OP_RETURN")

    //TODO N/A = 0x01 - 0x4b
    val OP_PUSHDATA1 = Value(0x4c, "OP_PUSHDATA1")
    val OP_PUSHDATA2 = Value(0x4d, "OP_PUSHDATA2")
    val OP_PUSHDATA4 = Value(0x4e, "OP_PUSHDATA4")
    val OP_1NEGATE = Value(0x4f, "OP_1NEGATE")

    val OP_TOALTSTACK = Value(0x6b, "OP_TOALTSTACK")
    val OP_FROMALTSTACK = Value(0x6c, "OP_FROMALTSTACK")
    val OP_IFDUP = Value(0x73, "OP_IFDUP")
    val OP_DEPTH = Value(0x74, "OP_DEPTH")
    val OP_DROP = Value(0x75, "OP_DROP")
    val OP_DUP = Value(0x76, "OP_DUP")
    val OP_NIP = Value(0x77, "OP_NIP")
    val OP_OVER = Value(0x78, "OP_OVER")
    val OP_PICK = Value(0x79, "OP_PICK")
    val OP_ROLL = Value(0x7a, "OP_ROLL")
    val OP_ROT = Value(0x7b, "OP_ROT")
    val OP_SWAP = Value(0x7c, "OP_SWAP")
    val OP_TUCK = Value(0x7d, "OP_TUCK")
    val OP_2DROP = Value(0x6d, "OP_2DROP")
    val OP_2DUP = Value(0x6e, "OP_2DUP")
    val OP_3DUP = Value(0x6f, "OP_3DUP")
    val OP_2OVER = Value(0x70, "OP_2OVER")
    val OP_2ROT = Value(0x71, "OP_2ROT")
    val OP_2SWAP = Value(0x72, "OP_2SWAP")

    //OP_CAT
    //OP_SUBSTR
    //OP_LEFT
    //OP_RIGHT
    val OP_SIZE = Value(0x82, "OP_SIZE")

    //OP_INVERT
    //OP_AND
    //OP_OR
    //OP_XOR
    val OP_EQUAL = Value(0x87, "OP_EQUAL")
    val OP_EQUALVERIFY = Value(0x88, "OP_EQUALVERIFY")

    val OP_1ADD = Value(0x8b, "OP_1ADD")
    val OP_1SUB = Value(0x8c, "OP_1SUB")
    //OP_2MUL
    //OP_2DIV
    val OP_NEGATE = Value(0x8f, "OP_NEGATE")
    val OP_ABS = Value(0x90, "OP_ABS")
    val OP_NOT = Value(0x91, "OP_NOT")
    val OP_NOTEQUAL = Value(0x92, "OP_NOTEQUAL")
    val OP_ADD = Value(0x93, "OP_ADD")
    val OP_SUB = Value(0x94, "OP_SUB")
    //OP_MUL
    //OP_DIV
    //OP_MOD
    //OP_LSHIFT
    //OP_RSHIFT
    val OP_BOOLAND = Value(0x9a, "OP_BOOLAND")
    val OP_BOOLOR = Value(0x9b, "OP_BOOLOR")
    val OP_NUMEQUAL = Value(0x9c, "OP_NUMEQUAL")
    val OP_NUMEQUALVERIFY = Value(0x9d, "OP_NUMEQUALVERIFY")
    val OP_NUMNOTEQUAL = Value(0x9e, "OP_NUMNOTEQUAL")
    val OP_LESSTHAN = Value(0x9f, "OP_LESSTHAN")
    val OP_GREATERTHAN = Value(0xa0, "OP_GREATERTHAN")
    val OP_LESSTHANOREQUAL = Value(0xa1, "OP_LESSTHANOREQUAL")
    val OP_GREATERTHANOREQUAL = Value(0xa2, "OP_GREATERTHANOREQUAL")
    val OP_MIN = Value(0xa3, "OP_MIN")
    val OP_MAX = Value(0xa4, "OP_MAX")
    val OP_WITHIN = Value(0xa5, "OP_WITHIN")

    val OP_RIPEMD160 = Value(0xa6, "OP_RIPEMD160")
    val OP_SHA1 = Value(0xa7, "OP_SHA1")
    val OP_SHA256 = Value(0xa8, "OP_SHA256")
    val OP_HASH160 = Value(0xa9, "OP_HASH160")
    val OP_HASH256 = Value(0xaa, "OP_HASH256")
    val OP_CODESEPARATOR = Value(0xab, "OP_CODESEPARATOR")
    val OP_CHECKSIG = Value(0xac, "OP_CHECKSIG")
    val OP_CHECKSIGVERIFY = Value(0xad, "OP_CHECKSIGVERIFY")
    val OP_CHECKMULTISIG = Value(0xae, "OP_CHECKMULTISIG")
    val OP_CHECKMULTISIGVERIFY = Value(0xaf, "OP_CHECKMULTISIGVERIFY")
    val OP_PUBKEYHASH = Value(0xfd, "OP_PUBKEYHASH")
    val OP_PUBKEY = Value(0xfe, "OP_PUBKEY")
    val OP_INVALIDOPCODE = Value(0xff, "OP_INVALIDOPCODE")

  }

}
