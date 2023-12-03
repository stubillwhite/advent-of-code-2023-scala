package adventofcode

import adventofcode.Day03.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day03Test extends AnyFlatSpec {

  private val exampleInput: String =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(4361)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(519444)
  }

  behavior of "solutionPartTwo"

  it should "return example result given example input" in {
    solutionPartTwo(exampleInput) should be(467835)
  }

  it should "return example result given problem input" in {
    solutionPartTwo(problemInput) should be(74528807)
  }
}
