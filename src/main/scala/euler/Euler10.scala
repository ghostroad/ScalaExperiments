package euler

import euler.Utils.primes

object Euler10 extends App {
  println(primes[BigInt].takeWhile(_ < 2000000).foldLeft(BigInt(0))(_ + _))
}