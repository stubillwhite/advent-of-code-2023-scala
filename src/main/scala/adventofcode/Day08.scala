package adventofcode

import adventofcode.Day08.Direction.*
import adventofcode.Utils.loadResourceAsString

object Day08 {

  val problemInput: String =
    loadResourceAsString("day-08-input.txt")

  def solutionPartOne(input: String): Long = {
    val instr = parseInput(input)
    val finalState = followInstructions(instr)
    finalState.steps
  }

  def main(args: Array[String]): Unit = {
    println(s"Part one: ${solutionPartOne(problemInput)}")
    println("Done")
  }

  private def followInstructions(instr: Instructions): State = {
    val nextState = (state: State) => {
      val idx = state.steps % instr.directions.size
      val dir = instr.directions(idx)

      val currNode = instr.nodes(state.currLocation)
      val newLocation = dir match {
        case Left => currNode.left
        case Right => currNode.right
      }

      state.copy(steps = state.steps + 1, currLocation = newLocation)
    }

    LazyList.iterate(State(0, "AAA"))(nextState)
      .dropWhile(_.currLocation != "ZZZ")
      .head
  }

  private case class State(steps: Int, currLocation: String)

  private def parseInput(s: String): Instructions = {
    Instructions(parseDirections(s), parseNodes(s))
  }

  private def parseDirections(s: String): Seq[Direction] = {
    s.split("\n").head.map(Direction.fromChar)
  }

  private def parseNodes(s: String): Map[String, Node] = {
    val pattern = raw"(\S+) = \((\S+), (\S+)\)".r

    s.split("\n")
      .drop(2)
      .map { case pattern(from, left, right) => from -> Node(left, right) }
      .toMap
  }

  object Direction extends Enumeration {
    type Direction = Value
    val Left, Right = Value

    def fromChar(ch: Char): Direction = if (ch == 'L') Left else Right
  }

  case class Node(left: String, right: String)

  case class Instructions(directions: Seq[Direction], nodes: Map[String, Node])
}