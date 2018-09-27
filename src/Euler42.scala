object Euler42 extends App {
  val words = io.Source.fromFile("/Users/ghostroad/IdeaProjects/ScalaExperiments/src/p042_words.txt").mkString.split(",").map(word => word.slice(1, word.length -1).toLowerCase)

  def isTriangleNumber(n: Int): Boolean = {
    val d = math.sqrt(2 * n)
    val a = math.floor(d)
    2 * n == a * (a + 1)
  }

  def value(word: String): Int = {
    word.map(letter => letter - 96).sum
  }

  println(words.count(word => isTriangleNumber(value(word))))
}
