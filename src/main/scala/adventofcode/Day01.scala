package adventofcode

import adventofcode.Utils.loadResourceAsString

object Day01 {

  val problemInput: String =
    loadResourceAsString("day-01-input.txt")

  def solutionPartOne(input: String): Int = {
    parseInput(input)
      .map(extractDigits(numeralDigitsExtractor))
      .map(calibrationValue)
      .sum
  }

  // Part two

  def solutionPartTwo(input: String): Int = {
    parseInput(input)
      .map(extractDigits(numeralAndWordDigitsExtractor))
      .map(calibrationValue)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private type DigitsExtractor = String => Seq[String]

  private val numeralDigitsExtractor: DigitsExtractor =
    s => {
      val pattern = raw"(\d)".r
      (for (m <- pattern.findAllMatchIn(s)) yield m.group(1)).toList
    }

  val numeralAndWordDigitsExtractor: DigitsExtractor =
    s => {
      val numerals = (1 to 9).map(_.toString)
      val words = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
      val numeralsAndWords = numerals ++ words

      val toNumeral = (s: String) =>
        if (words.contains(s)) (words.indexOf(s) + 1).toString else s

      val numeralsAndWordsPattern = numeralsAndWords.mkString("|")
      val pattern = s"(?=($numeralsAndWordsPattern))".r
      val matches = for (m <- pattern.findAllMatchIn(s)) yield m

      matches
        .toList
        .sortBy(_.start)
        .map(_.group(1))
        .map(toNumeral)
    }

  private def parseInput(input: String): Seq[String] = {
    input.split("\n")
  }

  def extractDigits(digitsExtractor: DigitsExtractor)(s: String): Seq[String] = {
    digitsExtractor(s)
  }

  private def calibrationValue(xs: Seq[String]): Int = {
    (xs.head + xs.last).toInt
  }
}
