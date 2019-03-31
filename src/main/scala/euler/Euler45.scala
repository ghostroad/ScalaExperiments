package euler

import euler.utils.Utils

object Euler45 extends App {
  def isHexagonal(n: Long): Boolean = Euler44.isPerfectSquare(1 + 8 * n) && Utils.divides(1 + math.sqrt(1 + 8 * n).toLong, 4)

  val triangleNumbers = from(1L).map(n => (n*(n+1))/2)

  def from(n: Long) : Stream[Long] = Stream.iterate(n)(_ + 1L)

  triangleNumbers.filter(n => isHexagonal(n) && Euler44.isPentagonal(n)).take(3).foreach(println)
}
