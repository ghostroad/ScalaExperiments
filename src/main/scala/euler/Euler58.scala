package euler

import euler.utils.Utils

object Euler58 extends App {

  def primeCount(n: Long): Long = {
      val start = (4 * n * n) + (4 * n) + 1
      List(start, start - (2 * n), start - (4 * n), start - (6 * n)).count(Utils.isPrime)
  }

  val result = Stream.iterate((1L, 3L)) { case (layer, count) => (layer + 1, count + primeCount(layer + 1)) }.find {
    case (layer, primeCount) => primeCount / ( 4.0 * layer + 1.0) < 0.1
  }
  println(result)
}
