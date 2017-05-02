package vendor

/**
  * @see [[ProgramParser]]
  */
object ProgramParserImpl extends ProgramParser {

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
        case hd :: _ if !isInstruction(hd) =>
          throw new InvalidInstructionFormatException("Unrecognised instruction")
        case hd :: tl if requiresArguments(hd) && tl.isEmpty =>
          throw new InvalidInstructionFormatException(s"$hd requires an argument")
        case hd :: tl if tl.nonEmpty => new Instruction(hd, tl.map {
          case n if n.matches("\\d+") => n.toInt
          case _ => throw new InvalidInstructionFormatException(s"$hd only accepts Int arguments")
        }.toVector)
        case hd :: _ => new Instruction(hd, Vector.empty)
      }
    }).toVector

  /**
    * Check if a String matching an instruction requires arguments
    * @param s The String to check
    * @return True if instruction requires arguments, false if not
    */
  private def requiresArguments(s: String): Boolean = s match {
    case "iconst" => true
    case _ => false
  }

  /**
    * Checks if a given string represents a recognised [[Instruction]]
    *
    * @param s The string to check
    * @return True if the string is a recognised [[Instruction]], else False
    */
  private def isInstruction(s: String): Boolean = s match {
    case "iconst" | "iadd" | "isub" | "imul" | "idiv" | "irem" |
         "ineg" | "idec" | "iinc" | "iswap" | "idup" | "print" => true
    case _ => false
  }
}
