package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Day01._

class Day01Test extends AnyFlatSpec {

  private val exampleInputOne =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInputOne) should be(142)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(53651)
  }

  behavior of "numeralAndWordDigitsExtractor"

  it should "return digits given overlaps" in {
    val actual = extractDigits(numeralAndWordDigitsExtractor)("1twoneighthreeskipsevenineightskip1")
    val expected = List(1, 2, 1, 8, 3, 7, 9, 8, 1).map(_.toString)
    actual should be(expected)
  }

  behavior of "solutionPartTwo"

  private val exampleInputTwo =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  it should "return example result given example input" in {
    solutionPartTwo(exampleInputTwo) should be(281)
  }

  it should "return example result given problem input" in {
    solutionPartTwo(problemInput) should be(53894)
  }
}
