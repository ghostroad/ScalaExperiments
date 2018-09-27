object Euler43 extends App {

  val primes = Array(2, 3, 5, 7, 11, 13, 17)
  def pandigitalNumbers = (0 to 9).permutations.map(v => BigInt(v.map(_.toString).mkString))

  def isWeird(n : BigInt):Boolean = {
    val nAsStr = n.toString
    nAsStr.length == 10 && (1 to 7).forall(i => (nAsStr.slice(i, i + 3).toInt % primes(i - 1) == 0))
  }

  println(pandigitalNumbers.filter(isWeird).sum)
}
