package playground

/**
 * Created by andrea on 25/05/15.
 */
object TypeClassPlayground {

  case class Expelt(stink:Double,size:Double)

  trait Expellable[T] {
    def doit(out:T):Expelt
  }

  case class Pee(stink:Double = 10/3,size:Double = 10/2) { for(i <- 0 to size.toInt) yield println("Peeing ") }
  case class Shit(stink:Double = 10/7,size:Double = 10/7) { for(i <- 0 to size.toInt) yield println("Shitting ") }

  implicit object PeeIsExpellable extends Expellable[Pee]{
    override def doit(out:Pee) = Expelt(out.stink,out.size)
  }

  implicit object ShitIsExpellable extends Expellable[Shit]{
    override def doit(out:Shit) = Expelt(out.stink,out.size)
  }

  /**
   * This functionality might be in other packages and the implicit Expellable implementation provided
   * at compile time by third parties
   * @param out
   * @param f
   * @tparam T
   * @return
   */
  def useToilet[T](out:T)(implicit f:Expellable[T]) = f.doit(out)

  def letsDoIt = {
    useToilet(Pee(10/3,10/2))

    println("LOL")

    useToilet(Shit(10/5,10/4))
  }

}
