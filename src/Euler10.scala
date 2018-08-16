import Utils._

object Euler10 extends App {
  println(primes.takeWhile(_ < 2000000).foldLeft(BigInt(0))(_ + _))
}
