package vendor

/**
  * @author lmignot
  */
object ProgramParserImpl extends ProgramParser {

  private lazy val legalInstructions = Vector(
    "iconst", "iadd", "isub", "imul", "idiv", "irem", "ineg",
    "idec", "iinc", "iswap", "idup", "print"
  )

  /**
    * @see [[vendor.ProgramParser.parse()]]
    */
  override def parse(file: String): InstructionList = {
    import scala.io.Source
    linesToInstructionList(Source.fromFile(file).getLines())
  }

  /**
    * @see [[vendor.ProgramParser.parseString()]]
    */
  override def parseString(string: String): InstructionList = {
    linesToInstructionList(string.split("\n").toIterator)
  }

  /**
    * Parses lines of Strings containing instructions and optional
    * arguments to an InstructionList
    *
    * @param lines Some Iterator containing Strings to parse
    * @return the parsed instructions in an [[InstructionList]]
    * @throws InvalidInstructionFormatException if parsing fails
    */
  private def linesToInstructionList(lines: Iterator[String]): InstructionList =
    lines.map(l => {
      l.split(" ").toList match {
        case hd :: _ if !legalInstructions.contains(hd) =>
          throw new InvalidInstructionFormatException("Unrecognised instruction")
        case hd :: tl if tl.nonEmpty => new Instruction(hd, tl.map {
          case n if n.matches("\\d+") => n.toInt
          case _ => throw new InvalidInstructionFormatException(s"$hd only accepts Int arguments")
        }.toVector)
        case hd :: _ => new Instruction(hd, Vector.empty)
      }
    }).toVector
}
