object Euler32 extends App {
  def isPandigitalTriple(a: Int, b: Int): Boolean = {
    val c = a * b
    (a.toString + b.toString + c.toString).sorted == "123456789"
  }

  for {a <- 1 to 9
       b <- 1234 to 9876} {
    if (isPandigitalTriple(a, b)) println(a, b, a * b)
  }

  for {a <- 12 to 98
       b <- 123 to 987} {
    if (isPandigitalTriple(a, b)) println(a, b, a * b)
  }


}
