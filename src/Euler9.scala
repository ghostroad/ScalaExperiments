object Euler9 extends App {
  def sq(n: Int):Int = scala.math.pow(n, 2).toInt
  def isPythagorean(a:Int, b:Int, c:Int) = sq(a) + sq(b) == sq(c)

  for (a <- 0 to 1000) {
    for (b <- a to (1000 - a)) {
      for (c <- b to (1000 - (a + b))) {
        if (a + b + c == 1000 & isPythagorean(a, b, c)) println(a*b*c)
      }
    }
  }
}
