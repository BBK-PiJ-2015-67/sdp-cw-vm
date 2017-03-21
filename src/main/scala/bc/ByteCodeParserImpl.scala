package bc

/**
  * Parses Bytes, using the injected [[ByteCodeFactory]]
  * to create [[ByteCode]] objects
  *
  * @param factory the [[ByteCodeFactory]] that will make the ByteCode objects
  *                once they've been parsed
  */
case class ByteCodeParserImpl(factory: ByteCodeFactory) extends ByteCodeParser {

  private lazy val hasArgs = Vector(bytecode("iconst"))

  /**
    * @see [[ByteCodeParser.parse]]
    */
  override def parse(bc: Vector[Byte]): Vector[ByteCode] = {
    def parser(out: Vector[ByteCode], in: Vector[Byte]): Vector[ByteCode] = in.toList match {
      case hd :: tl if hasArgs.contains(hd) => {
        case tl.nonEmpty => parser(out :+ factory.make(hd, tl.head), tl.tail.toVector)
        case _ => throw new InvalidBytecodeException(s"Argument missing for code matching byte: $hd")
      }
      case hd :: tl => parser(out :+ factory.make(hd), tl.toVector)
      case _ => out
    }
    parser(Vector.empty, bc)
  }
}
