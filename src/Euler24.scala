import scala.collection.immutable.SortedSet

object Euler24 extends App {
  def perms(digits: SortedSet[String]):Array[String] = {
    if (digits.isEmpty) {
      Array("")
    } else {
      digits.toArray.flatMap(digit => perms(digits - digit).map(digit + _))
    }
  }

  println(perms(SortedSet("0", "1", "2", "3", "4", "5", "6","7", "8", "9"))(999999))
}
