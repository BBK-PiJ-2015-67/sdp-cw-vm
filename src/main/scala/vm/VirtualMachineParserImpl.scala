package vm
import bc.{ByteCode, ByteCodeParser, InvalidBytecodeException}
import vendor.{Instruction, InvalidInstructionFormatException, ProgramParser}

/**
  * @see [[VirtualMachineParser]]
  */
case class VirtualMachineParserImpl(vParser: ProgramParser,
                                    bcParser: ByteCodeParser)
  extends VirtualMachineParser {

  type InstructionList = Vector[Instruction]
  type ByteCodeList = Vector[ByteCode]

  /**
    * @see [[VirtualMachineParser.parse()]]
    * @throws InvalidBytecodeException if parsing fails
    */
  override def parse(file: String): ByteCodeList =
    adapt(tryToParse(file, vParser.parse))

  /**
    * @see [[VirtualMachineParser.parseString()]]
    * @throws InvalidBytecodeException if parsing fails
    */
  override def parseString(str: String): ByteCodeList =
    adapt(tryToParse(str, vParser.parseString))

  /**
    * Catches [[InvalidInstructionFormatException]]
    * and throws the expected [[InvalidBytecodeException]]
    * instead
    *
    * @param input String containing codes to be parsed
    * @param f Function that takes a String and returns
    *          an [[InstructionList]]
    * @return The parsed [[InstructionList]] if successful
    * @throws InvalidBytecodeException if parsing fails
    */
  private def tryToParse(input: String,
                  f: String => InstructionList): InstructionList = {
    try f(input)
    catch {
      case ex: InvalidInstructionFormatException =>
        throw new InvalidBytecodeException(ex.getMessage)
    }
  }

  /**
    * Adapts an [[InstructionList]] to a [[ByteCodeList]]
    *
    * @param input [[InstructionList]] to adapt
    * @return [[ByteCodeList]] with each instruction adapted
    *         as a [[ByteCode]]
    */
  private def adapt(input: InstructionList): ByteCodeList = {
    var output: Vector[Byte] = Vector.empty
    for (el <- input) {
      output :+= bcParser.bytecode(el.name)
      if (el.args.nonEmpty) output = output ++ el.args.map(_.toByte)
    }
    bcParser.parse(output)
  }
}
