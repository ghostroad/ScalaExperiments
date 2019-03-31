package euler

import euler.utils.Utils

object Euler25 extends App {
  val fib: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: Utils.unfold((BigInt(0), BigInt(1))) {case (prevprev, prev) => (prevprev + prev, (prev, prevprev + prev))}

  fib.zipWithIndex.find({case(n, i) => n.formatted("%d").length >= 1000}) match {
    case Some(x) => println(x._2)
  }
}
