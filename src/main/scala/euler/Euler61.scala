package euler

object Euler61 extends App {
  def generateFigurates(formula: Int => Int): List[Int] = Stream.from(0).map(formula).dropWhile(_ < 1000).takeWhile(_ < 10000).toList
  val triangles = generateFigurates(n => n * (n + 1) / 2)
  val squares = generateFigurates(n => n * n)
  val pentagons = generateFigurates(n => (n * (3 * n - 1)) / 2)
  val hexagons = generateFigurates(n => n * ((2 * n) - 1))
  val heptagons = generateFigurates(n => (n * (5 * n - 3)) / 2)
  val octagons = generateFigurates(n => n * (3 * n - 2))

  def canBeAdjacent(a: Int, b: Int) = a.toString.takeRight(2) == b.toString.take(2)

  def canFollow(acc: List[Int], n: Int) = acc match {
    case Nil => true
    case last :: _ => canBeAdjacent(last, n)
  }

  def solve(remaining: List[List[Int]], acc: List[Int]): List[List[Int]] = remaining match {
    case Nil => List(acc)
    case _ =>
      val compatible = for {
        i <- remaining.indices.toList
        n <- remaining(i) if canFollow(acc, n)
      } yield (i, n)
      compatible.flatMap{ case (i, n) =>
          solve(remaining.take(i) ++ remaining.drop(i + 1), n :: acc)
      }
  }

  val candidates = solve(List(triangles, squares, pentagons, hexagons, heptagons, octagons), Nil)

  candidates.find { candidate =>
    val reversed = candidate.reverse
    canBeAdjacent(reversed.last, reversed.head)
  }.foreach(l => println(l.sum))
}
