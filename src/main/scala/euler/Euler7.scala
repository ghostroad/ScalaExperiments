package euler

import euler.utils.Utils.primes

object Euler7 extends App {

  println(primes[BigInt](implicitly)(10000))

}
