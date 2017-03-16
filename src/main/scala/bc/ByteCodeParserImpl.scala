package bc

/**
  * Parses Bytes, using the injected [[ByteCodeFactory]]
  * to create [[ByteCode]] objects
  *
  * @param factory the [[ByteCodeFactory]] that will make the ByteCode objects
  *                once they've been parsed
  */
case class ByteCodeParserImpl(factory: ByteCodeFactory) extends ByteCodeParser {

  private lazy val byteCodesWithArgs = Vector(bytecode("iconst"))

  /**
    * @see [[ByteCodeParser.parse]]
    */
  override def parse(bc: Vector[Byte]): Vector[ByteCode] = {
    var result = Vector.empty
    // pass each byte to the factory
    // unless the byte matches "iconst"
    // in which case it should take the following byte (to Int?) as argument
    // the factory handles error checking
    result
  }
}
