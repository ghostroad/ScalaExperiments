package euler

object Euler23 extends App {

  val u = 28124
  def isAbundant(n:Int) = Euler21.dMemo(n) > n

  val abundants = (2 to u).filter(isAbundant)

  val isAbundantSum = Array.fill(u)(false)

  for {a <- abundants
    b <- abundants} {
    if (a + b < u) isAbundantSum(a + b) = true
  }

  println((1 until u).filter(!isAbundantSum(_)).sum)


}
