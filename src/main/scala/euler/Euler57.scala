package euler

object Euler57 extends App {
  def next(frac: (BigInt, BigInt)): (BigInt, BigInt) = frac match { case (num, denom) =>
    val a = num - denom
    val b = denom
    (3*b + a, 2*b + a)
  }
  Stream.iterate((BigInt(3),BigInt(2)))(next).take(8).foreach(println)
  println(Stream.iterate((BigInt(3), BigInt(2)))(next).take(1000).count { case (num, denom) => num.toString.length > denom.toString.length })
}
