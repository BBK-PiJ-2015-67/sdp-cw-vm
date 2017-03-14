package vendor

/**
  * @author lmignot
  */
case class ProgramParserImpl() extends vendor.ProgramParser {

  private val ValidInstructions = Vector("iconst", "iconst", "iswap", "iadd", "iconst", "iadd",
    "iconst", "isub", "iconst", "imul", "iconst", "idiv",
    "iconst", "irem", "ineg", "idec", "iinc", "idup", "print", "print")

  /**
    * @see [[vendor.ProgramParser.parse()]]
    */
  override def parse(file: String): InstructionList = {
    import scala.io.Source
    parseLinesToVector(Source.fromFile(file).getLines())
  }

  /**
    * @see [[vendor.ProgramParser.parseString()]]
    */
  override def parseString(string: String): InstructionList = {
    parseLinesToVector(string.split("\n").toIterator)
  }

  /**
    * Parses an Iterator of Strings into
    * an InstructionList
    *
    * @param lines Some Iterator containing Strings to parse
    * @return the parsed instructions in an [[InstructionList]]
    */
  private def parseLinesToVector(lines: Iterator[String]): InstructionList =
    // TODO: Error checking etc
    lines
      .map(l => {
      val parts = l.split(" ")
      if (parts.length > 1) {
        new Instruction(parts.head, parts.tail.map(_.toInt).toVector)
      } else {
        new Instruction(parts.head, Vector())
      }
    })
    .toVector
}
