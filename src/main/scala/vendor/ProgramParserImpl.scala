package vendor

/**
  * @author lmignot
  */
object ProgramParserImpl extends ProgramParser {

  private lazy val ValidInstructions = Vector(
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
    * @throws InvalidInstructionFormatException if parsing fails
    */
  private def parseLinesToVector(lines: Iterator[String]): InstructionList =
    lines.map(l => {
      l.split(" ").toList match {
        case hd :: tl if tl.nonEmpty => new Instruction(hd, tl.map {
          case n if n.matches("\\d+") => n.toInt
          case _ => throw new InvalidInstructionFormatException(s"${hd} only accepts Int arguments")
        }.toVector)
        case hd :: _ if !ValidInstructions.contains(hd) =>
          throw new InvalidInstructionFormatException("Unrecognised instruction")
        case hd :: _ => new Instruction(hd, Vector.empty)
      }
    }).toVector
}
