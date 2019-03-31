package euler

object Euler37 extends App {
  def isLeftTruncatablePrime(n: BigInt): Boolean = {
    val nStr = n.toString
    (0 until nStr.length).map(nStr.drop(_).toInt).forall(Utils.isPrime)
  }

  def isRightTruncatablePrime(n: BigInt): Boolean = {
    val nStr = n.toString
    (1 to nStr.length).map(nStr.slice(0,_).toInt).forall(Utils.isPrime)
  }

  def isBiTruncatablePrime(n: BigInt): Boolean = {
    isLeftTruncatablePrime(n) && isRightTruncatablePrime(n)
  }

  println(Utils.from[BigInt](11).filter(isBiTruncatablePrime).take(11).sum)

}
