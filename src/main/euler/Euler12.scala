package euler

import euler.Utils.{from, sqrt}

object Euler12 extends App {
  def numDivisors(n: BigInt) = {
    val divisors = from(1).takeWhile(_ <= sqrt(n)).filter(n % _ == 0).flatMap(d => Array(n/d, d)).distinct
    divisors.size
  }


  println(from(1).map(n => (n * (n + 1))/2).find(n => numDivisors(n) > 500))

}
