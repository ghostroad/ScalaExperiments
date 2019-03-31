package euler

object Euler33 extends App {

  def isCurious(num: Int, denom: Int): Boolean = {
    val numStr = num.toString
    val denomStr = denom.toString
    num != denom && numStr(1) == denomStr(0) && (numStr(0).toString.toInt * denom) == (num * denomStr(1).toString.toInt)
  }

  for {a <- 10 to 99
       b <- 10 to 99} {
    if (isCurious(a, b)) println(a, b)
  }
}
