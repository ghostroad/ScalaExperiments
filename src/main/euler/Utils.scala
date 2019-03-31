package euler

import scala.collection.immutable.Stream.cons
import scala.math.BigInt

object Utils {

  trait NumberTheory[T] {
    def inc(n: T) : T

    def isPrime(n: T) = n match {
      case `one` => false
      case _ => primes.takeWhile(lt(_, sqrt(n))).forall(!divides(n, _))
    }

    def sqrt(n : T): T

    def lt(a : T, b : T): Boolean

    def divides(n: T, d: T) : Boolean

    val one : T

    lazy val primes: Stream[T] = inc(one) #:: succ(2, one) #:: from(succ(3, one))(this).filter(n => isPrime(n))

    def succ(n: Int, init: T): T = n match {
      case 0 => init
      case j => inc(succ(n - 1, init))
    }
  }

  implicit object BigIntNumberTheory extends NumberTheory[BigInt] {
    override def inc(n: BigInt): BigInt = n + 1

    override val one: BigInt = BigInt(1)

    override def sqrt(n: BigInt): BigInt = math.sqrt(n.doubleValue).toInt

    override def divides(n: BigInt, d: BigInt): Boolean = n % d == 0

    override def lt(a: BigInt, b: BigInt): Boolean = a <= b

  }

  implicit object LongNumberTheory extends NumberTheory[Long] {
    override def inc(n: Long): Long = n + 1

    override val one: Long = 1L

    override def sqrt(n: Long): Long = math.sqrt(n.doubleValue).toLong

    override def lt(a: Long, b: Long): Boolean = a <= b

    override def divides(n: Long, d: Long): Boolean = n % d == 0
  }

  implicit object IntNumberTheory extends NumberTheory[Int] {
    override def inc(n: Int): Int = n + 1

    override def sqrt(n: Int): Int = math.sqrt(n.doubleValue).toInt

    override def lt(a: Int, b: Int): Boolean = a <= b

    override def divides(n: Int, d: Int): Boolean = n % d == 0

    override val one: Int = 1
  }

  def primes[T](implicit numberTheory : NumberTheory[T]): Stream[T] = numberTheory.primes

  def isPrime[T](n: T)(implicit numberTheory : NumberTheory[T]): Boolean = numberTheory.isPrime(n)

  def from[T](n : T)(implicit numberTheory : NumberTheory[T]) : Stream[T] = Stream.iterate(n)(numberTheory.inc)

  def sqrt(n: BigInt) = math.floor(math.sqrt(n.doubleValue())).toInt

  def divides(n: Int, d: Int) = (n % d) == 0

  def divides(n: Long, d: Long) = (n % d) == 0

  def unfold[State, Output](initial: State)(transformation: State => (Output, State)): Stream[Output] = {
    val (output, newState) = transformation(initial)
    cons(output, Utils.unfold(newState)(transformation))
  }

}
