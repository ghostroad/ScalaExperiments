object Euler34 extends App {
  def isCurious(a: Int) = {
    a.toString.map {d => Euler20.factorial(d.toString.toInt)}.sum == a
  }
  for (a <- 1 to 2540160) {
      if (isCurious(a)) println(a)
  }
}
