package adventofcode

import adventofcode.Day06.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day06Test extends AnyFlatSpec {

  private val exampleInput: String =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(288)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(275724)
  }

  behavior of "solutionPartTwo"

  it should "return example result given example input" in {
    solutionPartTwo(exampleInput) should be(71503)
  }

  it should "return example result given problem input" in {
    solutionPartTwo(problemInput) should be(37286485)
  }
}
