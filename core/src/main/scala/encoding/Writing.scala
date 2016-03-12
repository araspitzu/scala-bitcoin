package encoding


/**
 * Created by andrea on 04/07/15.
 *
 * Move this object into Parsing ?
 */
object Writing {

  trait ByteWritable{

    def byteFormat:Array[Byte]

  }

}
