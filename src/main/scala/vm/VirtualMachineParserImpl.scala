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
    parseAndAdapt(file)(vParser.parse)(adapt)

  /**
    * @see [[VirtualMachineParser.parseString()]]
    * @throws InvalidBytecodeException if parsing fails
    */
  override def parseString(str: String): ByteCodeList =
    parseAndAdapt(str)(vParser.parseString)(adapt)

  /**
    * Parses and adapts an [[InstructionList]] to a [[ByteCodeList]]
    * @param input String containing codes to be parsed
    * @param f Function that takes a String and returns an [[InstructionList]]
    * @param g Function that takes an [[InstructionList]] and returns a [[ByteCodeList]]
    * @return [[ByteCodeList]] with each instruction adapted
    *         as a [[ByteCode]]
    * @throws InvalidBytecodeException if parsing fails
    */
  private def parseAndAdapt(input: String)
                           (f: String => InstructionList)
                           (g: InstructionList => ByteCodeList): ByteCodeList = {
    try g(f(input))
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
  private def adapt(input: InstructionList): ByteCodeList =
    bcParser.parse(input.foldLeft(Vector.empty)((acc, ins) => ins match {
      case Instruction if ins.args.nonEmpty => {
        acc :+ bcParser.bytecode(ins.name)
        acc :+ ins.args.map(_.toByte)
      }
      case Instruction if ins.args.isEmpty => acc :+ bcParser.bytecode(ins.name)
      case _ => acc
    }))
}
