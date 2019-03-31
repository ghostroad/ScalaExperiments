package euler

object Euler51 extends App {

  def tenToThe(pow: Int): Long = {
    if (pow == 0) 1 else {
      10 * tenToThe(pow - 1)
    }
  }

  def subLists[T](list: List[T]): List[List[T]] = {
    list match {
      case Nil => List(Nil)
      case first :: rest => {
        val restSublists = subLists(rest)
        restSublists ++ restSublists.map(first :: _)
      }
    }
  }

  def increments(n: Long, digit: Int): List[Long] = {
    val places = digits(n).zipWithIndex.filter(_._1 == digit).map(i => tenToThe(i._2))
    subLists(places).filter(_.nonEmpty).map(_.sum)
  }

  def digits(n: Long) = Stream.iterate(n)(_/10).takeWhile(_ != 0).map(_ % 10).toList

  def isReplacementBase(n: Long, numberOfReplacements: Int): Boolean = {
    (0 to (10 - numberOfReplacements)).exists { digit =>
      increments(n, digit).exists { increment =>
        (0 to (9 - digit)).map(i => (i * increment) + n).count(Utils.isPrime) == numberOfReplacements
      }
    }
  }

  println(Utils.primes[Long].find(isReplacementBase(_, 8)))

}
