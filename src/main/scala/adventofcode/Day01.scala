package adventofcode

import adventofcode.Utils.loadResourceAsString

object Day01 {

  val problemInput: String =
    loadResourceAsString("day-01-input.txt")

  def solutionPartOne(input: String): Int = {
    parseInput(input)
      .map(extractCalibrationValues)
      .map(calibrationValue)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println("Done")
  }

  private def parseInput(input: String): Seq[String] = {
    input.split("\n")
  }

  private def extractCalibrationValues(s: String): Seq[String] = {
    val pattern = raw"(\d)".r
    (for (m <- pattern.findAllMatchIn(s)) yield m.group(1)).toList
  }

  private def calibrationValue(xs: Seq[String]): Int = {
    (xs.head + xs.last).toInt
  }
}
