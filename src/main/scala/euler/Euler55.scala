package euler

object Euler55 extends App {
  def isPalindromic(n: BigInt) = n.toString.reverse == n.toString

  def nextInChain(n: BigInt) = n + BigInt(n.toString.reverse)

  def isLychrel(n: BigInt): Boolean = !Stream.iterate(nextInChain(n))(nextInChain).take(50).exists(isPalindromic)

  println((BigInt(1) to BigInt(9999)).count(isLychrel))
}