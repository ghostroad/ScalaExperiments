package euler

import euler.utils.Utils

object Euler41 extends App {

  def pandigitalNumbers(numDigits: Int) = (1 to numDigits).permutations.map(toInt)

  def toInt(digits: IndexedSeq[Int]) = {
    digits.map(_.toString).mkString.toInt
  }

  println((2 to 9).flatMap(numDigits => pandigitalNumbers(numDigits)).filter(Utils.isPrime).max)
}
