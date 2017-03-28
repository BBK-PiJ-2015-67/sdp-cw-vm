package bc
import bc.ByteCodes._

/**
  * @see [[ByteCodeFactory]]
  */
object ByteCodeFactoryImpl extends ByteCodeFactory with ByteCodeValues {

  /**
    * [[ByteCodeValues.bytecode]] maps a String representing a ByteCode
    * instruction to some Byte value. We require a reverse of this map so we can
    * match a Byte value against its String representation
    */
  private lazy val byteToNames: Map[Byte, String] = bytecode.map(_.swap)

  /**
    * @see [[ByteCodeFactory.make]]
    * @throws InvalidBytecodeException if parsing fails
    */
  override def make(byte: Byte, args: Int*): ByteCode = byteToNames.get(byte) match {
    case Some("iadd") => IAdd()
    case Some("isub") => ISub()
    case Some("imul") => IMul()
    case Some("idiv") => IDiv()
    case Some("irem") => IRem()
    case Some("ineg") => INeg()
    case Some("iinc") => IInc()
    case Some("idec") => IDec()
    case Some("iswap") => ISwap()
    case Some("idup") => IDup()
    case Some("print") => Print()
    case Some("iconst") if args.isEmpty =>
      throw new InvalidBytecodeException("IConst requires one argument")
    case Some("iconst") if args.length > 1 =>
      throw new InvalidBytecodeException("IConst accepts only one argument")
    case Some("iconst") => IConst(args.head)
    case _ => throw new InvalidBytecodeException(s"$byte is an invalid ByteCode")
  }
}
