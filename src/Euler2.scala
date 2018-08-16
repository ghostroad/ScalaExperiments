import scala.collection.immutable.Stream.cons

object Euler2 extends App{

  def unfold[State, Output](initial: State)(transformation: State => (Output, State)): Stream[Output] = {
    val (output, newState) = transformation(initial)
    cons(output, unfold(newState)(transformation))
  }


  val fib: Stream[Int] = 0 #:: unfold((0, 1)) {case (prevprev, prev) => (prevprev + prev, (prev, prevprev + prev))}

  println(fib.filter(_ % 2 == 0).takeWhile(_ < 4000000).foldLeft(0)(_ + _))
}
