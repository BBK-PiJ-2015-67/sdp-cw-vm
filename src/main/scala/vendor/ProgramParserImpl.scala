package vendor
import vendor.Instruction

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
    Source.fromFile(file)
      .getLines()
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

  /**
    * @see [[vendor.ProgramParser.parseString()]]
    */
  override def parseString(string: String): InstructionList = ???
}
