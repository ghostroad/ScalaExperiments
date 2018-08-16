object Euler1 extends App {
  val range = 1 until 1000
  val answer = range.filter(_ % 3 == 0).foldLeft(0)(_ + _) +
    range.filter(_ % 5 == 0).foldLeft(0)(_ + _) -
      range.filter(_ % 15==0).foldLeft(0)(_ + _)
  print(answer)

}
