package euler

object Euler36 extends App {
  def isBinaryPalindromic(n: Int) = {
    val binaryString = n.toBinaryString;
    binaryString == binaryString.reverse;
  }

  println((1 to 999999).filter(isBinaryPalindromic).filter(Euler4.isPalindromic).sum)
}
