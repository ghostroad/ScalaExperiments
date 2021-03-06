package euler

object Euler53 extends App {

  def chooseFactors(n: Long, r: Long): List[Double] = {
    val numerators = (r + 1) to n
    val denominators = 1L to (n - r)
    numerators.zipAll(denominators, 1L, 1L).toList.map { case (num, denom) => num.toDouble / denom }
  }

  def productExceeds(factors: List[Double], m: Long, acc: Double = 1): Boolean = {
    factors match {
      case Nil => false
      case first :: rest => {
        if (first * acc > m) true else productExceeds(rest, m, first * acc)
      }
    }
  }

  val count = (2 to 100).flatMap { n =>
    (2 to n).map((n, _))
  }.count { case (n, r) => productExceeds(chooseFactors(n, r), 1000000L) }

  println(count)
}
