package euler

object Euler20 extends App {
  def factorial(n: Int): BigInt = n match {
    case 0 => 1
    case _ => n * factorial(n - 1)
  }

  println(factorial(100).formatted("%d"))
  println(factorial(100).formatted("%d").split("").map(_.toInt).sum)
}
