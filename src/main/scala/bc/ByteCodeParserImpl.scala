package bc

/**
  * Parses Bytes, creating and returning equivalent ByteCode objects
  *
  * @param factory the ByteCodeFactory that will make the ByteCode objects
  *                once they've been parsed
  */
case class ByteCodeParserImpl(factory: ByteCodeFactory) extends ByteCodeParser {
  /**
    * @see [[ByteCodeParser.parse]]
    */
  override def parse(bc: Vector[Byte]): Vector[ByteCode] = ???
}
