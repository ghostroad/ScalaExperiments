package euler

object Euler35 extends App {
  def rotations(n: Int) = {
    val asStr = n.toString
    (1 to asStr.length).map(i => asStr.drop(i) + asStr.take(i)).map(_.toInt)
  }
  def isCircularPrime(n: Int) = {
    rotations(n).forall(Utils.isPrime)
  }

  println((2 to 1000000).count(isCircularPrime))
}
