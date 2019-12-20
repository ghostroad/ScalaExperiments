package euler

object Euler56 extends App {
  def exp(a: Int, b: Int): BigInt = BigInt(a).pow(b)
  def digitSum(n: BigInt): Int = n.toString.map(_.asDigit).sum

  println((for {
    a <- 1 to 100
    b <- 1 to 100
  } yield digitSum(exp(a, b))).max)
}
