
object Euler40 extends App {
  val initialSegment = "12345678910111213141516171819202122232425262728293031"
  val chars = for (i <- Stream.from(1); d <- i.toString) yield d
  def digit(n: Int): Int = {
    chars(n - 1).toString.toInt
  }

  println(digit(1) * digit(10) * digit(100) * digit(1000) * digit(10000) * digit(100000) * digit(1000000))


}
