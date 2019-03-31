package euler

object Euler46 extends App {
  def isGoldbachCounterExample(n : Long) : Boolean = {
    !Utils.isPrime(n) && n % 2 == 1 && Utils.primes[Long].takeWhile(p => (p < n)).forall(p => !Euler44.isPerfectSquare((n - p)/2))
  }

  println(Utils.from[Long](3L).find(isGoldbachCounterExample))
}
