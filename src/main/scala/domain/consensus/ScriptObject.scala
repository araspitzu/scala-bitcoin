package domain.consensus

import encoding.CommonParsersImplicits._
import encoding.Parsing.ParseFailure

import scala.util.control.ControlThrowable

/**
 * Created by andrea on 13/09/15.
 */
package object ScriptObject {

  val MAX_SCRIPT_ELEMENT_SIZE = Conf.ScriptConfig.MAX_SCRIPT_ELEMENT_SIZE

  // Threshold for nLockTime: below this value it is interpreted as block number,
  // otherwise as UNIX timestamp.
  val LOCKTIME_THRESHOLD = 500000000 // Tue Nov  5 00:53:20 1985 UTC

  /** Signature hash types/flags */
  case object SigHash extends Enumeration {
    val SIGHASH_ALL = Value(1, "SIGHASH_ALL")
    val SIGHASH_NONE = Value(2, "SIGHASH_NONE")
    val SIGHASH_SINGLE = Value(3, "SIGHASH_SINGLE")
    val SIGHASH_ANYONECANPAY = Value(0x80, "SIGHASH_ANYONECANPAY")
  }

  val OP_TRUE = Array(1.toByte)
  val OP_FALSE = Array(0.toByte)

  val uint256one = "0000000000000000000000000000000000000000000000000000000000000001".hex2bytes

  case object OP_CODES extends Enumeration {

    implicit val comparator = new {} with Ordering[OP_CODES.Value] {
      override def compare(x: OP_CODES.Value, y: OP_CODES.Value): Int = x.id.compare(y.id)
    }

    implicit def opcodesOrderingOps = comparator.mkOrderingOps _

    implicit def toByte(opcode:ScriptObject.OP_CODES.Value):Byte = opcode.id.toByte

    val OP_0 = Value(0x00, "OP_0")
    //0x01 - 0x4b OPCODE  emulating VarNumber, the number itself is the length of the data to push

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

    val OP_CAT = Value(0x7e, "OP_CAT")
    val OP_SUBSTR = Value(0x7f, "OP_SUBSTR")
    val OP_LEFT = Value(0x80, "OP_LEFT")
    val OP_RIGHT = Value(0x81, "OP_RIGHT")
    val OP_SIZE = Value(0x82, "OP_SIZE")

    val OP_INVERT = Value(0x83, "OP_INVERT")
    val OP_AND = Value(0x84, "OP_AND")
    val OP_OR = Value(0x85, "OP_OR")
    val OP_XOR = Value(0x86, "OP_XOR")
    val OP_EQUAL = Value(0x87, "OP_EQUAL")
    val OP_EQUALVERIFY = Value(0x88, "OP_EQUALVERIFY")

    val OP_1ADD = Value(0x8b, "OP_1ADD")
    val OP_1SUB = Value(0x8c, "OP_1SUB")
    val OP_2MUL = Value(0x8d, "OP_2MUL")
    val OP_2DIV = Value(0x8e, "OP_2DIV")
    val OP_NEGATE = Value(0x8f, "OP_NEGATE")
    val OP_ABS = Value(0x90, "OP_ABS")
    val OP_NOT = Value(0x91, "OP_NOT")
    val OP_NOTEQUAL = Value(0x92, "OP_NOTEQUAL")
    val OP_ADD = Value(0x93, "OP_ADD")
    val OP_SUB = Value(0x94, "OP_SUB")
    val OP_MUL = Value(0x95, "OP_MUL")
    val OP_DIV = Value(0x96, "OP_DIV")
    val OP_MOD = Value(0x97, "OP_MOD")
    val OP_LSHIFT = Value(0x98, "OP_LSHIFT")
    val OP_RSHIFT = Value(0x99, "OP_RSHIFT")
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

  case class ScriptError(msg:String) extends ControlThrowable {
    override def toString:String = msg
  }
  object ScriptError {
    def apply(failure: ParseFailure):ScriptError = ScriptError(failure.err)
  }

}
