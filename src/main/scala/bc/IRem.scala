package bc

/**
  * Push the sum of 2 Int values onto the stack
  *
  * @see [[MathByteCode]]
  */
case class IRem() extends MathByteCode(_ % _) {
  /**
    * @see [[bc.ByteCode.code]]
    */
  override val code: Byte = bytecode("iadd")
}
