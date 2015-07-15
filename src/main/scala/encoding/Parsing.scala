package encoding

import scala.util.{Failure, Success, Try}

/**
 * Created by andrea on 25/05/15.
 */
object Parsing {

  /**
   * Container for a successful or failed parse result
   * @tparam T
   */
  sealed trait ParseResult[+T]{

    def withOffset = this match {
      case ParseSuccess(result,used) => ParseSuccess( (result,used) , used )
      case e:ParseFailure => e
    }

     def get:(T,Int) = this match {
      case ParseSuccess(result,used) => (result,used)
      case e:ParseFailure => throw new RuntimeException(s"Value not parsed $e")
    }

    def getOrElse[U >: T](default: => U):(U,Int) = this match {
      case ParseSuccess(result,used) => (result,used)
      case e:ParseFailure =>(default,0)
    }

    def toOpt = this match {
      case ParseSuccess(result,_) => Some(result)
      case _ => None
    }

    def filter(test: T => Boolean):ParseResult[T] = this match {
      case ParseSuccess(result,used) if(test(result)) => ParseSuccess(result,used)
      case e:ParseFailure => e
    }


    //SHOULD BE def withFilter(test: T => Boolean):Iterator[ParseResult[T]]
//    def withFilter(test: T => Boolean):Iterator[ParseResult[T]] = new Iterator[ParseResult[T]]{
//      override def hasNext: Boolean = ???
//
//      override def next() = ???
//    }

    def map[U](f: T => U):ParseResult[U] = this match {
      case ParseSuccess(result,used) => flatMap( r =>  ParseSuccess(f(r),0))
      case e:ParseFailure => e
    }

    /**
     * Bind operation between two ParseResults carries the offset
     */
    def flatMap[U](f: T => ParseResult[U]):ParseResult[U] = this match {
        //IF THIS IS A SUCCESS
      case ParseSuccess(result,used) => {
        //Chain the parsing
        f(result) match {
            //If chained parsing is successful
          case ParseSuccess(newResult,newOffset) => ParseSuccess(newResult,used + newOffset)
          case e:ParseFailure => e
        }
      }
      case e:ParseFailure => e
    }

  }

  case class ParseSuccess[T](result: T,bytesUsed:Int) extends ParseResult[T]
  case class ParseFailure(err: String, thr: Option[Throwable]) extends ParseResult[Nothing]

    trait ByteReadable[T] {
      def read(bytes: Array[Byte], offset: Int): ParseResult[T]

      def safeRead(bytes: Array[Byte], offset: Int):ParseResult[T] = Try {
        read(bytes,offset)
      } match {
        case Success(t) => t
        case Failure(thr) => ParseFailure(s"Error parsing ${this.getClass.toString}",Some(thr))
      }

    }

    def parse[T](bytes:Array[Byte],offset:Int)(implicit reader:ByteReadable[T]):ParseResult[T] = reader.safeRead(bytes,offset)

    def parse[T](bytes:String)(implicit reader:ByteReadable[T]):ParseResult[T] = {
      import encoding.CommonParsersImplicits._
      parse[T](bytes.hex2bytes,0)
    }

    def parseList[T](bytes:Array[Byte],offset:Int,numElem:Int)(implicit reader:ByteReadable[T]):ParseResult[List[T]] = {
      val list = seqParse[T](bytes,offset,numElem)
      if(list.length < numElem)
        ParseFailure(s"Unable to parse $numElem elements, got only ${list.length}",None)
      else
        ParseSuccess(
          result = list.map(_.get._1),
          bytesUsed = list.foldLeft(0)( (acc,n) => acc + n.get._2)
        )
    }

    private def seqParse[T](bytes:Array[Byte],offset:Int,numElem:Int)(implicit reader:ByteReadable[T]):List[ParseResult[T]]= {
      if(numElem > 0)
        parse[T](bytes, offset) match {
          case ParseSuccess(t,used) => ParseSuccess(t,used)::seqParse[T](bytes,offset + used, numElem -1)
          case _:ParseFailure => Nil
        }
      else
        Nil
    }

  }
