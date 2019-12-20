package euler

import euler.utils.Utils

object Euler60 extends App {

  type T = Long

  def isPrime(n: T): Boolean = {
    if (n <= upperBound) primesCache(n) else Utils.isPrime(n)
  }

  def isGoodPair(m: T, n: T): Boolean = {
    isPrime((m.toString + n.toString).toLong) && isPrime((n.toString + m.toString).toLong)
  }

  val isGood = (n: T, existing: List[T]) => existing.forall(isGoodPair(n, _))

  val upperBound = 10000
  val primesCache = Utils.primes[T].takeWhile(_ <= 5 * upperBound).toSet
  val primeSequence = Utils.primes[T].takeWhile(_ <= upperBound).toList

  def findGoodSequences(len: Int, acc: List[T]): List[List[T]] = len match {
    case 0 => List(acc)
    case _ =>
      (acc.headOption match {
        case Some(m) => primeSequence.dropWhile(_ <= m)
        case None => primeSequence
      }).filter(isGood(_, acc)).flatMap(p => findGoodSequences(len - 1, p :: acc))
  }

  println(List(8389, 6733, 5701, 5197, 13).sum)
}
