object Euler38 extends App {
  def isPandigital(n: BigInt): Boolean = {
    n.toString.sorted == "123456789"
  }

  println((for (i <- 1 to 9999; n <- 2 to 9) yield
    BigInt((1 to n).map(j => (i * j).toString).mkString)).filter(isPandigital).max)
}
