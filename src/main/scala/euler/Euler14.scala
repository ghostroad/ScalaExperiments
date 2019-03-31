package euler

import euler.utils.Utils.from

import scala.collection.mutable

object Euler14 extends App {

  val one = BigInt(1)


  class CollatzLength {
    val memo = new mutable.HashMap[BigInt, BigInt]

    def value(n: BigInt): BigInt = memo.get(n)  match {
      case Some(m) => m
      case None => {n match {
        case `one` => 1
        case _ => {
          val result = 1 + value(collatzNext(n))
          memo.put(n, result)
          result
        }
      }
      }
    }

    def collatzNext(n: BigInt): BigInt = if (n % 2 == 0) n/2 else (3*n) + 1

  }


  private val col = new CollatzLength()

  println(from[BigInt](5).takeWhile(_ < 1000000).maxBy(col.value))

}
