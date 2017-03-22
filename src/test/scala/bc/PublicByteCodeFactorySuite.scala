package bc

import factory.VirtualMachineFactory
import org.scalatest.FunSuite

class PublicByteCodeFactorySuite extends FunSuite with ByteCodeValues {
  val bcf: ByteCodeFactory = VirtualMachineFactory.byteCodeFactory

  test("[9] all bytecodes should be made by factory") {
    // Tests that each bytecode (modulo "iconst") can be made.
    for ((_, code) <- (bytecode - "iconst")) {
      val bc = bcf.make(code)
      assert(bc.code == code, "invalid bytecode value")
    }

    // Test the iconst bytecode
    val bc = bcf.make(bytecode("iconst"), 4)
    assert(bc.code == bytecode("iconst"))
  }

  test("[10] an invalid bytecode should throw an exception") {
    intercept[InvalidBytecodeException] {
      bcf.make(99)
      bcf.make("asdf".toByte)
    }
  }

  test("[11] iconst without arguments should throw an exception") {
    intercept[InvalidBytecodeException] {
      bcf.make(bytecode("iconst"))
    }
  }

  test("[12] iconst with too many arguments should throw an exception") {
    intercept[InvalidBytecodeException] {
      bcf.make(bytecode("iconst"), 9, 34)
    }
  }

}
