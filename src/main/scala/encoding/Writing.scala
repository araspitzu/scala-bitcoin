package encoding

import java.io.OutputStream

/**
 * Created by andrea on 04/07/15.
 */
object Writing {

  trait ByteWritable[T]{

    def byteFormat[T](t:T):List[Byte]

  }

  def write[T](obj:T)(implicit writer:ByteWritable[T]):List[Byte] = writer.byteFormat(obj)

  def writeStream[T](obj:T)(implicit writer:ByteWritable[T]):Stream[Byte] = writer.byteFormat(obj).toStream

  def writeOutputStream[T](obj:T,writerOut:OutputStream)(implicit writer:ByteWritable[T]) = writerOut.write(writer.byteFormat(obj).toArray)
}
