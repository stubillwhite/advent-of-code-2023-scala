package adventofcode

import adventofcode.Utils.loadResourceAsString

object Day06 {

  case class Race(time: Long, record: Long)

  val problemInput: String =
    loadResourceAsString("day-06-input.txt")

  def solutionPartOne(input: String): Long = {
    parseInput(input)
      .map(limitsOfButtonTimeToBeatRecord)
      .map(countOfButtonTimes)
      .product
  }

  def solutionPartTwo(input: String): Long = {
    parseInputWithoutSpaces(input)
      .map(limitsOfButtonTimeToBeatRecord)
      .map(countOfButtonTimes)
      .product
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  // Solving for button-time x
  // d = (t - x) * x
  // 0 = -x^2 + tx -d
  //
  // (-b ± sqrt(b^2 - 4ac)) / 2a
  // a = -1, b = race-time, c = -dist

  private def parseInput(str: String): Seq[Race] = {
    val pattern = raw"\d+".r
    val Array(timesStr, recordsStr) = str.split("\n")

    val times = pattern.findAllIn(timesStr).map(_.toLong)
    val records = pattern.findAllIn(recordsStr).map(_.toLong)
    times.zip(records).map { case (t, r) => Race(t, r) }.toList
  }

  private def parseInputWithoutSpaces(str: String): Seq[Race] = {
    val pattern = raw"\d+".r
    val Array(timesStr, recordsStr) = str.split("\n").map(_.replaceAll(" ", ""))

    val times = pattern.findAllIn(timesStr).map(_.toLong)
    val records = pattern.findAllIn(recordsStr).map(_.toLong)
    times.zip(records).map { case (t, r) => Race(t, r) }.toList
  }

  private def quadraticRoots(a: Long, b: Long, c: Long): Seq[Double] = {
    val d = Math.sqrt((b * b) - (4 * a * c))
    val r1 = (-b + d) / (2 * a)
    val r2 = (-b - d) / (2 * a)
    Seq(r1, r2).sorted
  }

  private def limitsOfButtonTimeToBeatRecord(race: Race): Seq[Long] = {
    val Seq(r1, r2) = quadraticRoots(-1, race.time, -race.record)
    Seq(Math.floor(r1).toLong + 1, Math.ceil(r2).toLong - 1)
  }

  private def countOfButtonTimes(roots: Seq[Long]): Long = {
    val Seq(r1, r2) = roots
    1 + r2 - r1
  }
}