package euler

import scala.io.Source

object Euler22 extends App {
  def score(name: String) = {
    name.chars().map(_ - 64).sum
  }
  val contents:String = Source.fromFile(args(0)).getLines().next()
  val names = contents.split(",").sorted

  print(names.zipWithIndex.map({ case (name, rank) => score(name) * (rank + 1)}).sum)

}
