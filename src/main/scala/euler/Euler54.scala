package euler

object Euler54 extends App {
  case class Card(suite: String, rank: Int)

  object Card {
    def fromString(s: String): Card = {
      val strings = s.split("").toList
      strings match {
        case suite :: rankAsString :: Nil => {
          val rank = rankAsString match {
            case "A" => 14
            case "K" => 13
            case "Q" => 12
            case "J" => 11
            case num => num.toInt
          }
          Card(suite, rank)
        }
        case _ => throw new IllegalArgumentException(s"Couldn't parse $s")
      }
    }
  }

  case class Hand(cards: List[Card])

  object Hand {
    def fromStrings(s: List[String]): Hand = Hand(s.map(Card.fromString))
  }

}
