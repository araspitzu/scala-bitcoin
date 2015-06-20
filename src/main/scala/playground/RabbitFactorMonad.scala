package playground

/**
 * Created by andrea on 17/05/15.
 */
object RabbitFactorMonad {

  sealed trait Result[T]
  case class ResultSuccess[T](value:T) extends Result[T]
  case class ResultFailure(err:Throwable) extends Result[Nothing]

  /**
   *
   * @param value
   * @tparam T
   */
  case class RFactor[T](value:T) {
    def map[U](f: (T) => U): RFactor[U] = flatMap(x =>  RFactor(f(x)))

    def flatMap[U](f: (T) => RFactor[U]): RFactor[U] = {
      val factor = math.random
      val hiddenFactor = 2 ^ 16

      val newValue:T = (value.toString.length * factor * hiddenFactor toInt).asInstanceOf[T]

      println(s" Hey the rabbit calculated $newValue , old value was $value")

      f(newValue)
    }
  }

  /**
   *
   * @param value
   * @tparam T
   */

  case class RFactor1[T](value:T) {

    def map[U](f: (T) => U): RFactor1[U] = {
      val factor = math.random
      val hiddenFactor = 2 ^ 16

      val newValue:T = (value.toString.length * factor * hiddenFactor toInt).asInstanceOf[T]

      println(s" Hey the rabbit calculated $newValue , old value was $value")

      RFactor1(f(newValue))
    }

    def flatMap[U](f: (T) => RFactor1[U]): RFactor1[U] = map {  f(_).value  }
  }


  def provaFor = {

    for {
      z <- RFactor(Some(1))
      a <- RFactor(z)
      b <- RFactor(a)
    } yield {
      println(s"yielding $b")
    }

    for {
      z <- RFactor1(1)
      a <- RFactor1(z)
      b <- RFactor1(a)
    } yield {
      println(s"yielding $b")
    }


  }

}
