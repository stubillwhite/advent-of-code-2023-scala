package adventofcode

import adventofcode.Day02.Color.{Blue, Color, Green, Red}
import adventofcode.Utils.loadResourceAsString

object Day02 {

  val problemInput: String =
    loadResourceAsString("day-02-input.txt")

  def solutionPartOne(input: String): Int = {
    parseInput(input)
      .filter(isPossibleGame)
      .map(_.number)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println("Done")
  }

  private case class Game(number: Int, handfulls: Seq[Handfull])

  private type Handfull = Map[Color, Int]

  object Color extends Enumeration {
    type Color = Value
    val Red, Blue, Green = Value

    def fromString(s: String): Color = values.find(_.toString.toLowerCase.equals(s.toLowerCase)).get
  }

  private def parseInput(input: String): Seq[Game] = {
    input.split("\n").map(parseGame)
  }

  private def parseGame(s: String): Game = {
    val Array(gameStr, handfullsStr) = s.split(": ")
    Game(parseGameNumber(gameStr), parseHandfulls(handfullsStr))
  }

  private def parseGameNumber(s: String): Int = {
    val pattern = raw"Game (\d+)".r
    pattern.findFirstMatchIn(s).get.group(1).toInt
  }

  private def parseHandfulls(handfullsStr: String): Seq[Handfull] = {
    val parseHandfull = (s: String) => {
      val pattern = raw"(\d+) (\w+)".r

      pattern
        .findAllMatchIn(s)
        .map { case pattern(count, color) => Color.fromString(color) -> count.toInt }
        .toMap
    }

    handfullsStr.split("; ").map(parseHandfull)
  }

  private def isPossibleGame(game: Game): Boolean = {
    val isPossibleHandfull = (handfull: Handfull) => {
      handfull.getOrElse(Red, 0) <= 12 &&
        handfull.getOrElse(Green, 0) <= 13 &&
        handfull.getOrElse(Blue, 0) <= 14
    }

    game.handfulls.forall(isPossibleHandfull)
  }
}
