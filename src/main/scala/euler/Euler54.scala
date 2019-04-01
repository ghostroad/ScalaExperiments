package euler

import scala.io.Source

object Euler54 extends App {

  case class Card(suite: String, kind: Int)

  object Card {
    def fromString(s: String): Card = {
      val strings = s.split("").toList
      strings match {
        case kindAsString :: suite :: Nil => {
          val kind = kindAsString match {
            case "A" => 14
            case "K" => 13
            case "Q" => 12
            case "J" => 11
            case "T" => 10
            case num => num.toInt
          }
          Card(suite, kind)
        }
        case _ => throw new IllegalArgumentException(s"Couldn't parse $s")
      }
    }
  }

  case class RankedHand(hand: Hand, rank: Int, tiebreakers: List[Int])

  object FourOfAKind {
    def unapply(hand: Hand): Option[Int] = hand.hasNOfAKind(4)
  }

  object FullHouse {
    def unapply(hand: Hand): Option[(Int, Int)] = for {
      tripleKind <- hand.hasNOfAKind(3)
      pairKind <- hand.hasNOfAKind(2)
    } yield (tripleKind, pairKind)
  }

  object ThreeOfAKind {
    def unapply(hand: Hand): Option[Int] = hand.hasNOfAKind(3)
  }

  object TwoPairs {
    def unapply(hand: Hand): Option[(Int, Int)] = hand.hasTwoPairs
  }

  object OnePair {
    def unapply(hand: Hand): Option[Int] = hand.hasNOfAKind(2)
  }

  case class Hand(cards: List[Card]) {

    assert(cards.length == 5)

    def ranked: RankedHand = {
      val (rank, tiebreakers) = this match {
        case hand if hand.isRoyalFlush => (9, Nil)
        case hand if hand.isStraightFlush => (8, Nil)
        case FourOfAKind(kind) => (7, List(kind))
        case FullHouse(tripleKind, pairKind) => (6, List(tripleKind, pairKind))
        case hand if hand.isFlush => (5, Nil)
        case hand if hand.isStraight => (4, Nil)
        case ThreeOfAKind(tripleKind) => (3, List(tripleKind))
        case TwoPairs(higherKind, lowerKind) => (2, List(higherKind, lowerKind))
        case OnePair(kind) => (1, List(kind))
        case _ => (0, Nil)
      }

      RankedHand(this, rank, tiebreakers)
    }

    def isFlush: Boolean = cards.map(_.suite).forall(_ == cards.head.suite)

    def isStraight: Boolean = {
      val minRank = cards.minBy(_.kind).kind
      cards.map(_.kind).toSet == Set(minRank, minRank + 1, minRank + 2, minRank + 3, minRank + 4)
    }

    def distinctKinds: List[Int] = cards.map(_.kind).distinct

    def countByKind(kind: Int): Int = cards.count(_.kind == kind)

    def hasNOfAKind(n: Int): Option[Int] = distinctKinds.find(kind => countByKind(kind) == n)

    def isStraightFlush: Boolean = isStraight && isFlush

    def isRoyalFlush: Boolean = isStraightFlush && cards.maxBy(_.kind).kind == 14

    def hasTwoPairs: Option[(Int, Int)] = distinctKinds.filter(kind => countByKind(kind) == 2).sortWith(_ > _) match {
      case higherKind :: lowerKind :: Nil => Some(higherKind, lowerKind)
      case _ => None
    }

    def kindsInDescendingOrder: List[Int] = cards.map(_.kind).sortWith(_ > _)
  }

  object Hand {
    def fromStrings(s: List[String]): Hand = Hand(s.map(Card.fromString))
  }

  object Judge {

    private def compareKinds(tiebreakers1: List[Int], tiebreakers2: List[Int]): Option[Int] = {
      tiebreakers1.zip(tiebreakers2).collectFirst {
        case (k1, k2) if k1 > k2 => 1
        case (k1, k2) if k1 < k2 => 2
      }
    }

    val compareRankTiebreakers: (RankedHand, RankedHand) => Option[Int] = {
      case (hand1, hand2) =>
        compareKinds(hand1.tiebreakers, hand2.tiebreakers)
    }

    val compareRank: (RankedHand, RankedHand) => Option[Int] = {
      case (hand1, hand2) =>
        (hand1.rank, hand2.rank) match {
          case (r1, r2) if r1 > r2 => Some(1)
          case (r1, r2) if r1 < r2 => Some(2)
          case _ => None
        }
    }

    val compareHighKinds: (RankedHand, RankedHand) => Option[Int] = {
      case (hand1, hand2) =>
        compareKinds(hand1.hand.kindsInDescendingOrder, hand2.hand.kindsInDescendingOrder)
    }

    def winner(hand1: Hand, hand2: Hand): Option[Int] = {
      val ranked1 = hand1.ranked
      val ranked2 = hand2.ranked
      List(compareRank, compareRankTiebreakers, compareHighKinds)
        .map(f => f(ranked1, ranked2)).collectFirst { case Some(winner) => winner }
    }

  }

  val count: Int = Source.fromResource("p054_poker.txt").getLines().flatMap { line =>
    val cards = line.split(" ")
    val player1hand = Hand.fromStrings(cards.slice(0, 5).toList)
    val player2hand = Hand.fromStrings(cards.slice(5, 10).toList)
    Judge.winner(player1hand, player2hand)
  }.count(_ == 1)

  println(count)

}
