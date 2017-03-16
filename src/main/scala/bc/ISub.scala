package bc

/**
  * Push the difference of 2 Int values onto the stack
  *
  * @see [[MathByteCode]]
  */
case class ISub() extends MathByteCode(_ - _) {
  /**
    * @see [[bc.ByteCode.code]]
    */
  override val code: Byte = bytecode("isub")
}
