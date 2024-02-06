package adventofcode

import adventofcode.Day08.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day08Test extends AnyFlatSpec {

  private val exampleInputOne: String =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  private val exampleInputTwo: String =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input one" in {
    solutionPartOne(exampleInputOne) should be(2)
  }

  it should "return example result given example input two" in {
    solutionPartOne(exampleInputTwo) should be(6)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(20659)
  }
}
