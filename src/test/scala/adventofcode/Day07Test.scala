package adventofcode

import adventofcode.Day07.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day07Test extends AnyFlatSpec {

  private val exampleInput: String =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(6440)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(250898830)
  }

  behavior of "solutionPartTwo"

  it should "return example result given example input" in {
    solutionPartTwo(exampleInput) should be(5905)
  }

  it should "return example result given problem input" in {
    solutionPartTwo(problemInput) should be(252127335)
  }
}
