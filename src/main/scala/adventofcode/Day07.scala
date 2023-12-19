package adventofcode

import adventofcode.Day07.HandType._
import adventofcode.Utils.loadResourceAsString
import math.Ordering.Implicits.seqOrdering

object Day07 {

  val problemInput: String =
    loadResourceAsString("day-07-input.txt")

  case class Hand(cards: Seq[Char], bid: Long)

  private val exampleInput: String =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  def solutionPartOne(input: String): Long = {
    val hands = parseInput(input)

    hands
      .sorted(Ordering.by(h => handRank(h) +: cardRanks(h, false)))
      .zipWithIndex
      .map((hand, rank) => hand.bid * (rank + 1))
      .sum
  }

  def solutionPartTwo(input: String): Long = {
    val hands = parseInput(input)

    hands
      .sorted(Ordering.by(h => handRank(bestHand(h)) +: cardRanks(h, true)))
      .zipWithIndex
      .map((hand, rank) => hand.bid * (rank + 1))
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private def parseInput(s: String): Seq[Hand] = {
    val parseHand = (s: String) => {
      val Array(cards, bid) = s.split(" ")
      Hand(cards.toList, bid.toLong)
    }

    s.split("\n").map(parseHand)
  }

  private def handRank(h: Hand): Int = {
    val ranks = List(HIGH_CARD, ONE_PAIR, TWO_PAIR, THREE_OF_A_KIND, FULL_HOUSE, FOUR_OF_A_KIND, FIVE_OF_A_KIND)
    ranks.indexOf(handType(h))
  }

  private def handType(h: Hand): HandType = {
    val byFrequency = h.cards.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2).reverse

    byFrequency match {
      case (_, 5) :: _ => FIVE_OF_A_KIND
      case (_, 4) :: _ => FOUR_OF_A_KIND
      case (_, 3) :: (_, 2) :: _ => FULL_HOUSE
      case (_, 3) :: _ => THREE_OF_A_KIND
      case (_, 2) :: (_, 2) :: _ => TWO_PAIR
      case (_, 2) :: _ => ONE_PAIR
      case _ => HIGH_CARD
    }
  }

  private def cardRanks(hand: Hand, withJokers: Boolean): Seq[Int] = {
    val ranks = if (withJokers) "J23456789TQKA".toList else "23456789TJQKA".toList
    hand.cards.map(ranks.indexOf)
  }

  private def bestHand(hand: Hand): Hand = {
    val nonJokerCards = hand.cards.filter(_ != 'J')
    val byFrequency = nonJokerCards.groupBy(identity).view.mapValues(_.size).toList.sortBy(_._2).reverse
    val mostCommonCard = byFrequency.map(_.head).headOption.getOrElse('K')

    val newCards = hand.cards.mkString.replaceAll("J", mostCommonCard.toString).toList

    hand.copy(cards = newCards)
  }

  object HandType extends Enumeration {
    type HandType = Value
    val FIVE_OF_A_KIND, FOUR_OF_A_KIND, FULL_HOUSE, THREE_OF_A_KIND, TWO_PAIR, ONE_PAIR, HIGH_CARD = Value
  }
}