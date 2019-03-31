object Euler52 extends App {

  def digitHistogram(n: Long) = {

  }
  def isPermutationSequence(first: Long, length: Int): Boolean = {
    val digits = first.toString.sorted
    (1 to length).forall(i => (first * i).toString.sorted == digits)
  }

  println(Utils.from[Long](1).find(isPermutationSequence(_, 6)))

}
