package adventofcode

import adventofcode.Utils.loadResourceAsString

import scala.annotation.tailrec

object Day05 {

  case class Almanac(seeds: Seq[Long], categories: Seq[Category])

  case class Category(name: String, mappings: Seq[Mapping])

  case class Mapping(range: ClosedRange, offset: Long)

  case class ClosedRange(start: Long, end: Long)

  val problemInput: String =
    loadResourceAsString("day-05-input.txt")

  def solutionPartOne(input: String): Long = {
    val almanac = parseInput(input)

    almanac.seeds
      .map(seed => almanac.categories.foldLeft(seed)(translatePoint))
      .min
  }

  // This really hurts my head, need to write it out. Turn the input into intervals and step through the stages, splitting
  // the intervals if required at each stage, then take the final minimum value. See tests for TDD at each stage.

  def solutionPartTwo(input: String): Long = {
    val almanac = parseInput(input)
    val translatedRanges = translateRanges(almanac)

    translatedRanges.head.start
  }

  def parseInput(input: String): Almanac = {
    input.split("\n\n").toList match {
      case seedsStr :: categoriesStr =>
        Almanac(parseSeeds(seedsStr), parseCategories(categoriesStr))
    }
  }

  def translateRanges(almanac: Almanac) = {
    val seedRanges = almanac.seeds
      .sliding(2, 2)
      .map { case Seq(a, b) => ClosedRange(a, a + b - 1) }
      .toList
      .sortBy(_.start)

    val translateRanges = (ranges: Seq[ClosedRange], cat: Category) => {
      println("----------------------------------------------------------")
      println(s"${cat.name}")
      ranges.flatMap(translateRange(_, cat)).sortBy(_.start)
    }

    almanac.categories.foldLeft(seedRanges)(translateRanges)
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println(s"Part two: ${solutionPartTwo(problemInput)}")
    println("Done")
  }

  private def parseSeeds(s: String): Seq[Long] = {
    val pattern = raw"(\d+)".r
    val matches = for {m <- pattern.findAllMatchIn(s)} yield m.group(0)
    matches.toList.map(_.toLong)
  }

  private def parseCategories(strs: Seq[String]): Seq[Category] = {
    val namePattern = raw"(\S+) map:".r

    for {
      str <- strs
      name = namePattern.findFirstMatchIn(str).get.group(1)
      mappings = parseMappings(str)
    } yield Category(name, mappings)
  }

  private def parseMappings(str: String): Seq[Mapping] = {
    val pattern = raw"(\d+) (\d+) (\d+)".r

    val mappings = for {
      matches <- pattern.findAllMatchIn(str)
    } yield {
      matches.subgroups.map(_.toLong) match {
        case List(dst, src, len) => Mapping(ClosedRange(src, src + len - 1), dst - src)
      }
    }

    mappings.toList.sortBy(_.range.start)
  }

  private def fillGapsWithIdentityMapping(mappings: Seq[Mapping]): Seq[Mapping] = {

    @tailrec
    def fillGaps(endOfPrev: Long, processed: Seq[Mapping], remaining: Seq[Mapping]): Seq[Mapping] = {
      remaining match {
        case Nil => processed
        case m :: ms =>
          val padding = if (endOfPrev == m.range.start - 1) List() else List(Mapping(ClosedRange(endOfPrev + 1, m.range.start - 1), 0))
          fillGaps(m.range.end, processed ++ padding ++ List(m), ms)
      }
    }

    val contiguousMappings = fillGaps(-1, List(), mappings)

    val endOfAll = contiguousMappings.last.range.end
    val endPadding = if (endOfAll == Long.MaxValue) List() else List(Mapping(ClosedRange(endOfAll + 1, Long.MaxValue), 0))

    contiguousMappings ++ endPadding
  }

  @tailrec
  private def rangesAreOverlapping(a: ClosedRange, b: ClosedRange): Boolean = {
    if (b.start < a.start) rangesAreOverlapping(b, a) else (b.start <= a.end)
  }

  private def translateRange(range: ClosedRange, category: Category): Seq[ClosedRange] = {

    @tailrec
    def iter(src: ClosedRange, translated: Seq[ClosedRange], mappings: List[Mapping]): Seq[ClosedRange] = {
      mappings match {
        case m :: ms =>
          if (rangesAreOverlapping(src, m.range)) {
            val newStart = translatePoint(src.start, category)
            val newEnd = translatePoint(Math.min(src.end, m.range.end), category)

            val dst = ClosedRange(newStart, newEnd)

            val newSrc = if (src.end > m.range.end) src.copy(start = m.range.end + 1) else src

            iter(newSrc, translated ++ List(dst), ms)
          }
          else {
            iter(src, translated, ms)
          }

        case Nil =>
          translated
      }
    }

    val filled = fillGapsWithIdentityMapping(category.mappings).toList
    println(s"Input:    ${range}")
    println(s"Mappings: ${category.mappings.toList}")
    println(s"Filled:   ${filled}")
    val result = iter(range, List(), filled)
    println(s"Output:   ${result}")
    println()
    result
  }

  private def translatePoint(x: Long, category: Category): Long = {
    val mappings = category.mappings

    mappings.find(m => (m.range.start <= x) && (x <= m.range.end)) match {
      case Some(mapping) => x + mapping.offset
      case None => x
    }
  }
}