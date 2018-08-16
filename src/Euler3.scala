import scala.collection.immutable.Stream.cons
import scala.math.BigInt

object Euler3 extends App {
  val bigNumber:BigInt = 600851475143L

  val one = BigInt(1)

  def from(n: BigInt): Stream[BigInt] = cons(n, from(n+1))

  def sqrt(n: BigInt) = math.floor(math.sqrt(n.doubleValue())).toInt

  def isPrime(n: BigInt): Boolean = n match {
    case `one` => false
    case _ => val upperBound = math.floor(math.sqrt(n.doubleValue()))
      Stream.from(2).takeWhile(_ <= upperBound).forall(n % _ != 0)
  }

  println(from(BigInt(2)).takeWhile(_ <= sqrt(bigNumber)).filter(isPrime).filter(bigNumber % _ == 0).max)

}
