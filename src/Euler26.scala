object Euler26 extends App{
  def remSeq(d: Int): Stream[Int] = Stream.iterate(1) {
    prevRem => (prevRem * 10) % d
  }

  def recCycleLength(d: Int): Int = {
    val remsSeen = Array.fill[Option[Int]](d)(None)

    for ((rem, i) <- remSeq(d).zipWithIndex) {
      remsSeen(rem) match {
        case Some(j) => return i - j
        case None => remsSeen(rem) = Some(i)
      }
    }

    -1
  }

  println((2 to 999).maxBy(recCycleLength))
}
