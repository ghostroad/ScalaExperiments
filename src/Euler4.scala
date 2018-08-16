object Euler4 extends App {
  val upperLimit = 999 * 999
  def isPalindromic(n: Int) = n.toString.reverse == n.toString

  println((999 to 1 by -1).flatMap(i => (999 to 1 by -1).map(j => i * j)).filter(isPalindromic).max)

}
