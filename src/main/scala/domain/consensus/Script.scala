package domain.consensus

import java.security.NoSuchAlgorithmException

import crypto.Hash._
import crypto.{ECKeyPair, TransactionSignature}
import domain.{Transaction, VersionedChecksummed}
import domain.consensus.ScriptObject._
import domain.consensus.ScriptObject.ScriptError
import domain.consensus.ScriptObject.OP_CODES
import domain.consensus.ScriptObject.OP_CODES._
import encoding.CommonByteConverters._
import encoding.EnrichedTypes._
import encoding.Parsing._
import encoding.Writing.ByteWritable
import scala.util.{Failure, Success, Try}

/**
 * Created by andrea on 04/07/15.
 */
case class Script(bytes: Array[Byte]) extends ByteWritable {
  type OP_CODE = OP_CODES.Value
  type Stack = List[Array[Byte]]
  type Chunk = Either[OP_CODE, Array[Byte]]
  type ParsedScript = List[Chunk]

  /**
   * Utility class adding functionalities to the Chunk type
   * @param value the chunk to be "enriched"
   */
  implicit class EnrichedChunk(value: Chunk) {

    def length = value match {
      case Left(op_code) => op_code.id
      case Right(data) => data.length
    }

    def byteFormat:Array[Byte] = value match {
      case Left(op_code) => Array(op_code.toByte)
      case Right(data) => data
    }

  }

  lazy val parsedScript: ParsedScript = Try {
     parseScript(bytes)
    } match {
     case Success(result) => result
     case Failure(parseError:ScriptError) => throw parseError
     case Failure(thr) => throw new ScriptError(thr.getMessage)
    }

  def length:Int = bytes.length

  override def byteFormat:Array[Byte] = bytes

  override def toString:String = parsedScript.map {
    case Left(op_code) => op_code.toString + " "
    case Right(data) => data.bytes2hex + " "
  }.mkString.dropRight(1)

  def isSendToAddress:Boolean = parsedScript match {
    case Left(OP_DUP) ::
         Left(OP_HASH160) ::
         Right(data) ::
         Left(OP_EQUALVERIFY) ::
         Left(OP_CHECKSIG) :: Nil => true
    case _ => false
  }

  /**
   * See <a href="https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki">BIP 16</a>
   * @return
   */
  def isPayToScriptHash:Boolean = {
    bytes.length == 23 &&
    OP_CODES(bytes(0) & 0xff) == OP_HASH160 &&
    (bytes(1) & 0xff) == 0x14  &&
    OP_CODES(bytes(22) & 0xff) == OP_EQUAL
  }

  private def eval(txContainingThis:Transaction, txInputIndex:Int, script:ParsedScript):Boolean =
    run(txContainingThis, txInputIndex,  script).headOption map castToBool getOrElse false



  def getPubKeyHash:Array[Byte] = {
    if(isSendToAddress)
      parsedScript(2).byteFormat
    else if(isPayToScriptHash)
      parsedScript(1).byteFormat
    else
      throw new ScriptError("Script not in scriptPubKey form")
  }

  def getPubKey:Array[Byte] = {

    if(parsedScript.length != 2)
      throw new ScriptError(s"Malformed script for getPubKey, expected size 2 got ${parsedScript.length}")

    val chunk0 = parsedScript.head
    val chunk1 = parsedScript.tail.head

    if(chunk0.length > 2 && chunk1.length > 2)
      chunk1.byteFormat
    else if(chunk1 == Left(OP_CHECKSIG) && chunk0.length > 2)
      chunk0.byteFormat
    else
      throw new ScriptError("Malformed script")

  }

  private def run(txContainingThis: Transaction,
                  txInputIndex:Int,
                  script:ParsedScript,
                  initialStack: Stack = List.empty):Stack = script.zipWithIndex.foldLeft[Stack](initialStack) {

    case ( stack, (Right(data), index) ) =>
      if(data.length > MAX_SCRIPT_ELEMENT_SIZE)
        throw new ScriptError(s"Data push exceeded $MAX_SCRIPT_ELEMENT_SIZE bytes")

      data :: stack
    case ( stack, (Left(op_code), index) ) => op_code match {
      case op_numeric if (op_numeric >= OP_0 && op_numeric <= OP_16) =>
        Array(op_numeric.toByte) :: stack

      case OP_DUP => topOrFail(stack, op_code){ top =>
        top :: stack
      }
      // Crypto OP_CODES
      case OP_SHA1 => topOrFail(stack, op_code) { top =>
        sha1(top) :: stack.drop(1)
      }
      case OP_SHA256 => topOrFail(stack, op_code) { top =>
        sha256(top) :: stack.drop(1)
      }
      case OP_HASH256 => topOrFail(stack, op_code) { top =>
        hash256(top) :: stack.drop(1)
      }
      case OP_HASH160 => topOrFail(stack, op_code) { top =>
        hash160(top) :: stack.drop(1)
      }
      case OP_EQUAL |
           OP_EQUALVERIFY => pop2OrFail(stack, op_code){ (a, b) =>

        if (a.bytes2hex == b.bytes2hex)
          if(op_code == OP_EQUALVERIFY)
            stack.drop(2)
          else
            ScriptObject.OP_TRUE :: stack.drop(2)
        else if(op_code == OP_EQUALVERIFY)
          throw new ScriptError("Invalid script, OP_EQUALVERIFY not passed")
        else
          OP_FALSE :: stack.drop(2)
      }
      case OP_CHECKSIG |
           OP_CHECKSIGVERIFY => pop2OrFail(stack, op_code) { (pubKey, sigBytes) =>


        val lastCodeSepLocation =
          index -                                           //this position minus the difference from
          script                                            //here to last OP_CODESEPARATOR
            .dropRight(script.length - index)               //drop what's after this position
            .reverse                                        //start from this position
            .indexOf(Left(OP_CODESEPARATOR))                //index of the first found

        val subsetScript =
          script
            .drop(lastCodeSepLocation + 1)                  //drop the part before last OP_CODESEPARATOR and the opcode itself
            .filterNot {                                    //remove sigBytes from script
            case Right(data) => data.deep == sigBytes.deep
            case _ => false
          }

        val subsetScriptBytes = subsetScript.flatMap(_.byteFormat).toArray

        Try {
          val txSig = TransactionSignature(sigBytes)
          val sigHash = txContainingThis.hashForSignature(txInputIndex, subsetScriptBytes, txSig.sigHashFlags, false)

        } match {
          case Success(value) =>
          case Failure(ScriptError(errMsg)) =>
          case Failure(nsa:NoSuchAlgorithmException) => throw new ScriptError(nsa.getMessage)
          case Failure(thr) =>
        }

        OP_TRUE :: stack.drop(2)
      }
      case OP_ADD => pop2OrFail(stack, op_code) { (a, b) =>
        val op1 = ScriptNumber(a, false)
        val op2 = ScriptNumber(b, false)
        ScriptNumber(op1.int + op2.int).byteFormat :: stack.drop(2)
      }
      case x => throw new ScriptError(s"Unknown opcode $x")
    }
  }

  private def pop2OrFail(stack: Stack, op_code: OP_CODE)(f: (Array[Byte], Array[Byte]) => Stack): Stack = {
    stack match {
      case top::subTop::tail => f(top, subTop)
      case top::Nil => throw new ScriptError(s"Not enough arguments for $op_code")
      case Nil => throw new ScriptError(s"$op_code on empty stack")
    }
  }

  private def topOrFail(stack: Stack, op_code: OP_CODE)(f: Array[Byte] => Stack):Stack = {
    stack.headOption match {
      case Some(top) => f(top)
      case None => throw new ScriptError(s"$op_code on empty stack")
    }
  }

  private def parseScript(bytes:Array[Byte]):ParsedScript = bytes.headOption match {
    case None => List.empty[Chunk] //TODO throw exception ?
    case Some(byte) => byte unsigned match {
      case b if(isOpPush(b)) => Right(bytes.slice(1, 1 + b)) :: parseScript(bytes.drop(1 + b))
      case b => OP_CODES(b) match {
        case OP_PUSHDATA1 => parse[Short](bytes.tail,0) match {
          case ParseSuccess(len,used) => Right(bytes.slice(1 + used, 1 + used + len)) :: parseScript(bytes.drop(1 + used + len))
          case f:ParseFailure => throw ScriptError(f)
        }
        case OP_PUSHDATA2 => parse[Int](bytes.tail,0) match {
          case ParseSuccess(len,used) => Right(bytes.slice(1 + used, 1 + used + len)) :: parseScript(bytes.drop(1 + used + len))
          case f:ParseFailure => throw ScriptError(f)
        }
        case OP_PUSHDATA4 => parse[Long](bytes.tail,0) match {
          case ParseSuccess(len,used) => Right(bytes.slice(1 + used, 1 + used + len.toInt)) :: parseScript(bytes.drop(1 + used + len.toInt))
          case f:ParseFailure => throw ScriptError(f)
        }
        case OP_CAT |
             OP_SUBSTR |
             OP_LEFT |
             OP_RIGHT |
             OP_INVERT |
             OP_AND |
             OP_OR |
             OP_XOR |
             OP_2MUL |
             OP_2DIV |
             OP_MUL |
             OP_DIV |
             OP_MOD |
             OP_LSHIFT  => throw new ScriptError("Encountered disabled OPCODE")
        case op_n => Left(op_n) :: parseScript(bytes.tail)
      }
     }
  }

  private def isOpPush(b:Int) = b > 0x00 && b < 0x4c //0x00 until 0x4c contains b

  private def castToBool(data: Array[Byte]): Boolean = data.exists( b =>
    b.toInt != 0 &&
      !( b == data.last &&
        (b.toInt & 0xff) == 0x80 )
  )

}
