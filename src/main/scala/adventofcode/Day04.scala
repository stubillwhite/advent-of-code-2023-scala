package adventofcode

import adventofcode.Utils.loadResourceAsString

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day04 {

  private case class Card(n: Int, cardNumbers: Seq[Int], winningNumbers: Seq[Int], copies: Int) {
    val matches: Set[Int] = cardNumbers.toSet.intersect(winningNumbers.toSet)
  }

  val problemInput: String =
    loadResourceAsString("day-04-input.txt")

  def solutionPartOne(input: String): Int = {
    parseInput(input)
      .map(c => Math.pow(2, c.matches.size - 1).toInt)
      .sum
  }

  // Part two

  def solutionPartTwo(input: String): Int = {
    scoreCards(parseInput(input).toList)
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private def parseInput(input: String): Seq[Card] = {
    input.split("\n").map(parseCard)
  }

  private def parseCard(s: String): Card = {
    val pattern = raw".* (\d+): (.*) \| (.*)".r
    val m = pattern.findFirstMatchIn(s).get
    Card(m.group(1).toInt, parseNumbers(m.group(2)), parseNumbers(m.group(3)), 1)
  }

  private def parseNumbers(s: String) = {
    val pattern = raw"(\d+)".r
    (for (m <- pattern.findAllIn(s)) yield m.toInt).toList
  }

  private def scoreCards(cards: List[Card]): Int = {
    @tailrec
    def iter(n: Int, cards: List[Card]): Int =
      cards match {
        case Nil => n
        case c :: cs => iter(n + c.copies, copyNextN(cs, c.matches.size, c.copies))
      }

    iter(0, cards)
  }

  private def copyNextN(cards: List[Card], n: Int, times: Int): List[Card] = {
    val updatedCards = cards
      .take(n)
      .map(c => c.copy(copies = c.copies + times))

    updatedCards ++ cards.drop(n)
  }
}
