package adventofcode

import adventofcode.Day02.Color.{Blue, Color, Green, Red}
import adventofcode.Utils.loadResourceAsString

object Day02 {

  private case class Game(number: Int, handfuls: Seq[Handful])

  private type Handful = Map[Color, Int]

  object Color extends Enumeration {
    type Color = Value
    val Red, Blue, Green = Value

    def fromString(s: String): Color = values.find(_.toString.toLowerCase.equals(s.toLowerCase)).get
  }

  val problemInput: String =
    loadResourceAsString("day-02-input.txt")

  def solutionPartOne(input: String): Int = {
    parseInput(input)
      .filter(isPossibleGame)
      .map(_.number)
      .sum
  }

  // Part two

  def solutionPartTwo(input: String): Int = {
    parseInput(input)
      .map(minimumCubes)
      .map(powerset)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private def parseInput(input: String): Seq[Game] = {
    input.split("\n").map(parseGame)
  }

  private def parseGame(s: String): Game = {
    val Array(gameStr, handfulsStr) = s.split(": ")
    Game(parseGameNumber(gameStr), parseHandfuls(handfulsStr))
  }

  private def parseGameNumber(s: String): Int = {
    val pattern = raw"Game (\d+)".r
    pattern.findFirstMatchIn(s).get.group(1).toInt
  }

  private def parseHandfuls(handfulsStr: String): Seq[Handful] = {
    val parseHandful = (s: String) => {
      val pattern = raw"(\d+) (\w+)".r

      pattern
        .findAllMatchIn(s)
        .map { case pattern(count, color) => Color.fromString(color) -> count.toInt }
        .toMap
    }

    handfulsStr.split("; ").map(parseHandful)
  }

  private def isPossibleGame(game: Game): Boolean = {
    val isPossibleHandful = (handful: Handful) => {
      handful.getOrElse(Red, 0) <= 12 &&
        handful.getOrElse(Green, 0) <= 13 &&
        handful.getOrElse(Blue, 0) <= 14
    }

    game.handfuls.forall(isPossibleHandful)
  }

  private def mergeWith[K, V](f: (V, V) => V, a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    (a.keySet ++ b.keySet).map { k =>
      val v = (a.get(k), b.get(k)) match {
        case (Some(aVal), None) => aVal
        case (None, Some(bVal)) => bVal
        case (Some(aVal), Some(bVal)) => f(aVal, bVal)
      }
      k -> v
    }.toMap
  }

  private def minimumCubes(game: Game): Handful = {
    game.handfuls.reduce((a, b) => mergeWith(Integer.max, a, b))
  }

  private def powerset(handful: Handful): Int = {
    handful.values.product
  }
}
