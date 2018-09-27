
import scala.collection.immutable.Stream.cons
import scala.math.BigInt

object Utils {
  lazy val primes: Stream[BigInt] = BigInt(2) #:: BigInt(3) #:: from(BigInt(5)).filter(n => isPrime(n))

  val one = BigInt(1)

  def isPrime(n: BigInt) = n match {
    case `one` => false
    case _ => primes.takeWhile(_ <= sqrt(n)).forall(n % _ != 0)
  }

  def isPrime(n: Int) = n match {
    case 1 => false
    case _ => primes.takeWhile(_ <= sqrt(n)).forall(n % _ != 0)
  }


  def from(n: BigInt): Stream[BigInt] = cons(n, Utils.from(n+1))

  def sqrt(n: BigInt) = math.floor(math.sqrt(n.doubleValue())).toInt

  def divides(n: Int, d: Int) = (n % d) == 0

  def unfold[State, Output](initial: State)(transformation: State => (Output, State)): Stream[Output] = {
    val (output, newState) = transformation(initial)
    cons(output, Utils.unfold(newState)(transformation))
  }

}
