package adventofcode

import adventofcode.Utils.loadResourceAsString

import scala.util.matching.Regex

object Day03 {

  private case class Coordinates(x: Int, y: Int)

  private case class Word(startCoordinates: Coordinates, content: String) {
    val allCoordinates: Seq[Coordinates] =
      (startCoordinates.x until startCoordinates.x + content.length).map(x => Coordinates(x, startCoordinates.y))
  }

  val problemInput: String =
    loadResourceAsString("day-03-input.txt")

  def solutionPartOne(input: String): Int = {
    val numbers = extractWordsMatching(input, raw"\d+".r)
    val symbols = extractWordsMatching(input, raw"[^\d.]".r)

    val symbolToAdjacentNumbers = numbers.groupBy(adjacentSymbol(_, symbols))

    symbolToAdjacentNumbers
      .filter { case (sym, nums) => sym.isDefined }
      .flatMap { case (sym, nums) => nums.map(_.content.toInt)}
      .sum
  }

  def solutionPartTwo(input: String): Int = {
    val numbers = extractWordsMatching(input, raw"\d+".r)
    val symbols = extractWordsMatching(input, raw"[^\d.]".r)

    val symbolToAdjacentNumbers = numbers.groupBy(adjacentSymbol(_, symbols))

    symbolToAdjacentNumbers
      .filter { case (sym, nums) => sym.isDefined }
      .filter { case (sym, nums) => sym.get.content == "*" && nums.size == 2 }
      .map { case (sym, nums) => nums.map(_.content.toInt).product }
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private def extractWordsMatching(input: String, pattern: Regex): Seq[Word] = {
    for {
      (line, y) <- input.split("\n").toList.zipWithIndex
      m <- pattern.findAllMatchIn(line)
    } yield Word(Coordinates(m.start, y), m.group(0))
  }

  private def adjacentCoordinates(c: Coordinates): Seq[Coordinates] = {
    for {
      dx <- -1 to 1
      dy <- -1 to 1
    } yield Coordinates(c.x + dx, c.y + dy)
  }

  private def adjacentSymbol(number: Word, symbols: Seq[Word]): Option[Word] = {
    val allAdjacentCoordinates = number.allCoordinates.flatMap(adjacentCoordinates).toSet
    symbols.find(sym => allAdjacentCoordinates.contains(sym.startCoordinates))
  }
}
