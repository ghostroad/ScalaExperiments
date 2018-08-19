object Euler4 extends App {
  val upperLimit = 999 * 999
  def isPalindromic(n: Int) = n.toString.reverse == n.toString

  println((for (i <- 999 to 1 by -1; j <- 999 to 1 by -1) yield {
    i * j
  }).filter(isPalindromic).max)
}
