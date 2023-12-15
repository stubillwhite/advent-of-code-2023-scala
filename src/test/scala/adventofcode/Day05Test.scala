package adventofcode

import adventofcode.Day05.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day05Test extends AnyFlatSpec {

  val exampleInput: String =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  behavior of "solutionPartOne"

  it should "return example result given example input" in {
    solutionPartOne(exampleInput) should be(35)
  }

  it should "return example result given problem input" in {
    solutionPartOne(problemInput) should be(174137457)
  }

  behavior of "translateRanges"

  // seeds
  // output-intervals  [55 67] [79 92]
  //
  // seeds-to-soil
  // maps              {:start 50 :end 97 :offset 2} {:start 98 :end 99 :offset -48}
  //                   [55 67 2] [79 92 2]
  // output-intervals  [57 69]   [81 94]

  it should "return translated ranges given first translation" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(57, 69), ClosedRange(81, 94))
    translateRanges(almanac.copy(categories = almanac.categories.take(1))) should be(expected)
  }

  // soil-to-fertilizer
  // input-intervals   [57 69] [81 94]
  // maps              {:start 0, :end 14, :offset 39} {:start 15, :end 51, :offset -15} {:start 52, :end 53, :offset -15}
  // output-intervals  [57 69] [81 94]

  it should "return translated ranges given first two translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(57, 69), ClosedRange(81, 94))
    translateRanges(almanac.copy(categories = almanac.categories.take(2))) should be(expected)
  }

  // fertilizer-to-water
  // input-intervals   [57 69] [81 94]
  // maps              {:start 0, :end 6, :offset 42} {:start 7, :end 10, :offset 50} {:start 11, :end 52, :offset -11} {:start 53, :end 60, :offset -4}
  //                   [57 60 -4] [61 69 0] [81 94 0]
  // output-intervals  [53 56]    [61 69]   [81 94]

  it should "return translated ranges given first three translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(53, 56), ClosedRange(61, 69), ClosedRange(81, 94))
    translateRanges(almanac.copy(categories = almanac.categories.take(3))) should be(expected)
  }

  // water-to-light
  // input-intervals   [53 56] [61 69] [81 94]
  // maps              {:start 18, :end 24, :offset 70} {:start 25, :end 94, :offset -7}
  //                   [53 56 -7] [61 69 -7] [81 94 -7]
  // output-intervals  [46 49]    [54 62]    [74 87]

  it should "return translated ranges given first four translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(46, 49), ClosedRange(54, 62), ClosedRange(74, 87))
    translateRanges(almanac.copy(categories = almanac.categories.take(4))) should be(expected)
  }

  // light-to-temperature
  // input-intervals   [46 49] [54 62] [74 87]
  // maps              {:start 45, :end 63, :offset 36} {:start 64, :end 76, :offset 4} {:start 77, :end 99, :offset -32}
  //                   [46 49 36] [54 62 36] [74 76 4] [77 87 -32]
  //                   [82 85]    [90 98]    [78 80]   [45 55]
  // output-intervals  [45 55] [78 80] [82 85] [90 98]

  it should "return translated ranges given first five translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(45, 55), ClosedRange(78, 80), ClosedRange(82, 85), ClosedRange(90, 98))
    translateRanges(almanac.copy(categories = almanac.categories.take(5))) should be(expected)
  }

  // temperature-to-humidity
  // input-intervals   [45 55] [78 80] [82 85] [90 98]
  // maps              {:start 0, :end 68, :offset 1} {:start 69, :end 69, :offset -69}
  //                   [45 55 1] [78 80 0] [82 85 0] [90 98 0]
  // output-intervals  [46 56] [78 80] [82 85] [90 98]

  it should "return translated ranges given first six translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(46, 56), ClosedRange(78, 80), ClosedRange(82, 85), ClosedRange(90, 98))
    translateRanges(almanac.copy(categories = almanac.categories.take(6))) should be(expected)
  }

  // humidity-to-location
  // input-intervals   [46 56] [78 80] [82 85] [90 98]
  // maps              {:start 56, :end 92, :offset 4} {:start 93, :end 96, :offset -37}
  //                   [46 55 0] [56 56 4] [78 80 4] [82 85 4] [90 92 4] [93 96 -37] [97 98 0]
  // output-intervals  [46 55]   [60 60]   [82 84]   [86 89]   [94 96]   [56 59]     [97 98]
  //                   [46 55] [56 59] [60 60] [82 84] [86 89] [94 96] [97 98]

  it should "return translated ranges given all seven translations" in {
    val almanac = parseInput(exampleInput)
    val expected = List(ClosedRange(46, 55), ClosedRange(56, 59), ClosedRange(60, 60), ClosedRange(82, 84), ClosedRange(86, 89), ClosedRange(94, 96), ClosedRange(97, 98))
    translateRanges(almanac.copy(categories = almanac.categories)) should be(expected)
  }

  behavior of "solutionPartTwo"

  it should "return example result given example input" in {
    solutionPartTwo(exampleInput) should be(46)
  }

  it should "return example result given problem input" in {
    solutionPartTwo(problemInput) should be(1493866)
  }
}
