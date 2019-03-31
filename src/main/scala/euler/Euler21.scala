package euler

import euler.utils.Utils.sqrt

import scala.collection.mutable

object Euler21 extends App {
  def properDivisors(n: Int) = Stream.from(2).takeWhile(_ <= sqrt(n)).filter(n % _ == 0).flatMap(d => Array(n/d, d)).distinct

  def dMemo = {
    val cache = new mutable.HashMap[Int, Int]

    { n:Int => cache.get(n) match {
      case Some(m) => m
      case None => {
        val result = properDivisors(n).sum + 1
        cache.put(n, properDivisors(n).sum + 1)
        result
      }
    }}
  }

  def isAmicable(n:Int):Boolean = {
    val m = dMemo(n)
    m != n & dMemo(m) == n
  }

  println(isAmicable(220))
  println(isAmicable(284))
  println(isAmicable(10000))

  println((2 to 10000).filter(isAmicable).sum)
}
