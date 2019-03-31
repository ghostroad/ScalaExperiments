package euler

import euler.utils.Utils

object Euler44 extends App {

  def nthPentagonal(n: Long) : Long = (n*((3*n) - 1))/2

  def isPerfectSquare(j: Long) : Boolean = math.pow(math.floor(math.sqrt(j)), 2) == j

  def isPentagonal(i: Long) = isPerfectSquare(1 + 24*i) && Utils.divides((math.sqrt(1 + 24 * i) + 1).toLong, 6)

  def pentagonalsUntilDifferenceExceeded(difference: Long) =
    (1L to ((difference - 1)/3 + 1)).map(nthPentagonal)

  val pentagonals = Stream.iterate(1L)(x => x + 1).map(nthPentagonal)

  for { difference <- pentagonals
        a <- pentagonalsUntilDifferenceExceeded(difference)
       if (isPentagonal(a + difference) && isPentagonal(2*a + difference)) } {
    println(a, difference, a + difference)
    println(isPentagonal(a), isPentagonal(difference), isPentagonal(a + difference))
  }



}
