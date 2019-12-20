package euler

import scala.io.Source

object Euler59 extends App {
  val codes = Source.fromResource("p059_cipher.txt").getLines.next.split(",").map(_.toInt)
  def decrypt(key: List[Int]): String = {
    codes.zip(Stream.continually(key.toStream).flatten).map { case (inputChar, keyChar) => inputChar ^ keyChar }.map(_.toChar).mkString
  }
  val keyChars = 'a'.toInt to 'z'.toInt

  val containsWords = (input: String) => input.contains("Euler")

  val decrypted = (for {
    k1 <- keyChars
    k2 <- keyChars
    k3 <- keyChars
  } yield List(k1, k2, k3)).map(decrypt).find(containsWords).get

  println(decrypted.toCharArray.map(_.toInt).sum)

}
