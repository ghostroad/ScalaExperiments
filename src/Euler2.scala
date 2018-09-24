
object Euler2 extends App{


  val fib: Stream[Int] = 0 #:: Utils.unfold((0, 1)) {case (prevprev, prev) => (prevprev + prev, (prev, prevprev + prev))}

  println(fib.filter(_ % 2 == 0).takeWhile(_ < 4000000).sum)
}
