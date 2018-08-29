object Euler1 extends App {
  val range = 1 until 1000
  val answer = range.filter(_ % 3 == 0).sum +
    range.filter(_ % 5 == 0).sum -
      range.filter(_ % 15 == 0).sum
  print(answer)

}
