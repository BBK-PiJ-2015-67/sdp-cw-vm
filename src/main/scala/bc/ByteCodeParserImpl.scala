package bc

/**
  * Parses Bytes, using the injected [[ByteCodeFactory]]
  * to create [[ByteCode]] objects
  *
  * @param factory the [[ByteCodeFactory]] that will make the ByteCode objects
  *                once they've been parsed
  * @see [[ByteCodeParser]]
  */
case class ByteCodeParserImpl(factory: ByteCodeFactory) extends ByteCodeParser {

  private lazy val hasArgs = Vector(bytecode("iconst"))

  /**
    * @see [[ByteCodeParser.parse]]
    * @throws InvalidBytecodeException if parsing fails
    */
  override def parse(bc: Vector[Byte]): Vector[ByteCode] = {
    def parser(out: Vector[ByteCode], in: Vector[Byte]): Vector[ByteCode] = in match {
      case hd +: tl if hasArgs.contains(hd) =>
        if (tl.nonEmpty) {
          parser(out :+ factory.make(hd, tl.head), tl.tail)
        } else {
          throw new InvalidBytecodeException(s"Argument missing for code matching byte: $hd")
        }
      case hd +: tl => parser(out :+ factory.make(hd), tl)
      case _ => out
    }
    parser(Vector.empty, bc)
  }
}
