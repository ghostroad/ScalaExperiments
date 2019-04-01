package euler
import Euler54._
import org.scalatest.{FreeSpec, Matchers}

class Euler54Spec extends FreeSpec with Matchers {

  "Ranking a hand" - {
    "should work for a royal flush" in {
      Hand.fromStrings(List("AS", "JS", "KS", "QS", "TS")).ranked.rank shouldBe 9
    }

    "should work for a flush" in {
      Hand.fromStrings(List("2S", "JS", "KS", "QS", "TS")).ranked.rank shouldBe 5
    }

    "should work for two pairs" in {
      Hand.fromStrings(List("2S", "1S", "2H", "1C", "TD")).ranked.rank shouldBe 2
    }
  }

  "Judging a winner" - {
    "should work" in {
      def hand(s: String) = Hand.fromStrings(s.split(" ").toList)

      Judge.winner(hand("5H 5C 6S 7S KD"), hand("2C 3S 8S 8D TD")) shouldBe Some(2)
      Judge.winner(hand("5D 8C 9S JS AC"), hand("2C 5C 7D 8S QH")) shouldBe Some(1)
      Judge.winner(hand("2D 9C AS AH AC"), hand("3D 6D 7D TD QD")) shouldBe Some(2)
      Judge.winner(hand("4D 6S 9H QH QC"), hand("3D 6D 7H QD QS")) shouldBe Some(1)
      Judge.winner(hand("2H 2D 4C 4D 4S"), hand("3C 3D 3S 9S 9D")) shouldBe Some(1)
    }
  }

}
