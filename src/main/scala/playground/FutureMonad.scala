package playground

import java.util.concurrent.Callable

/**
 * Created by andrea on 23/05/15.
 */
object FutureMonad {

//  case class Futuro[+T](payload:T){
//
//    val callable = new {} with Callable[T] {
//      override def call(): T = payload
//    }
//
//    def map[U](f: T => U):Futuro[U] = flatMap( v => Futuro(f(v)) )
//
//    def flatMap[U](f: T => Futuro[U]):Futuro[U] = {
//
//
//      f(callable.call())
//    }
//
//  }
//
//  def testIt = {
//
//    val f = Futuro[String] {
//      Thread.sleep(3000)
//      "Hello world"
//    }
//
//    println("Hello now")
//  }
}
