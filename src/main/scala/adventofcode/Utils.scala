package adventofcode

import scala.io.Source

object Utils {

  def loadResourceAsString(name: String): String = {
    Source.fromResource(name).getLines().mkString("\n")
  }

}
