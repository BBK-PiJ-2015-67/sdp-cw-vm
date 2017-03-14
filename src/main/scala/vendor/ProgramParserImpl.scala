package vendor

/**
  * @author lmignot
  */
case class ProgramParserImpl() extends vendor.ProgramParser {

  private val ValidInstructions = Vector(
    "iconst", "iadd", "isub", "imul", "idiv", "irem", "ineg",
    "idec", "iinc", "iswap", "idup", "print"
  )

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
    lines
      .map(l => {
        val parts = l.split(" ")
        if (!ValidInstructions.contains(parts.head)) {
          throw new InvalidInstructionFormatException("Unrecognised instruction")
        }
        if (parts.length > 1) {
          val args = parts.tail.map {
            case n if n.matches("\\d+") => n.toInt
            case _ => throw new InvalidInstructionFormatException(s"${parts.head} only accepts Int arguments")
          }.toVector
          new Instruction(parts.head, args)
        } else {
          new Instruction(parts.head, Vector())
        }
      })
      .toVector
}
