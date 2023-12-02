package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Day02._

class Day02Test extends AnyFlatSpec {

  private val exampleInput: String =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(8)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(2716)
  }
}
