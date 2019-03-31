package euler

object Euler39 extends App {
  val counts = Array.fill(1001)(0)
  for (a <- 1 to 333; b <- (a+1) to (1000 - a)/2; c <- (b+1) to (1000 - (a + b))) {
    if (a*a + b*b == c*c) counts(a + b + c) += 1
  }

  println((1 to 1000).maxBy(counts(_)))
}
