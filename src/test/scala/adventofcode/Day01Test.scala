package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Day01._

class Day01Test extends AnyFlatSpec {

  private val exampleInput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(142)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(53651)
  }
}
