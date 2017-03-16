package bc

/**
  * Push the sum of 2 Int values onto the stack
  *
  * @see [[MathByteCode]]
  */
case class IDiv() extends MathByteCode(_ / _) {
  /**
    * @see [[bc.ByteCode.code]]
    */
  override val code: Byte = bytecode("idiv")
}
