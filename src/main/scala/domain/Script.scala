package domain

import encoding.Parsing.{ParseSuccess, ParseResult, ByteReadable}

/**
 * Created by andrea on 04/07/15.
 */
case class Script(bytes:Array[Byte]) {

  def length = bytes.length

}

object Script {


}
