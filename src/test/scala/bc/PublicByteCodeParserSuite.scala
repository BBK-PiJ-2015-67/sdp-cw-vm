package bc

import factory.VirtualMachineFactory
import org.scalatest.FunSuite
import bc.byteCodes.IConst

class PublicByteCodeParserSuite extends FunSuite with ByteCodeValues {
  val bcp: ByteCodeParser = VirtualMachineFactory.byteCodeParser

  test("[5] byte code parser should parse a single bytecode") {
    val code = Vector(bytecode("iadd"))
    val bc = bcp.parse(code)
    assert(bc.length == 1, "did not parse one bytecode")
    assert(bc(0).code == bytecode("iadd"), "did not have the correct code")
  }

  test("[6] byte code parser should parse a sequence of bytecode") {
    val code = Vector(bytecode("iconst"), 4.toByte, bytecode("iconst"), 5.toByte, bytecode("iadd"))
    val bc = bcp.parse(code)
    assert(bc.length == 3, "did not parse four bytecodes")
    assert(bc(0).code == bytecode("iconst"))
    assert(bc(1).code == bytecode("iconst"))
    assert(bc(2).code == bytecode("iadd"))
    assert(bc(0).asInstanceOf[IConst].num == 4)
    assert(bc(1).asInstanceOf[IConst].num == 5)
  }

  test("[7] byte code parser should throw if no args are provided to IConst") {
    val code1 = Vector(bytecode("iconst"))
    val code2 = Vector(bytecode("iconst"), bytecode("iadd"))
    intercept[InvalidBytecodeException] {
      bcp.parse(code1)
    }
  }

  test("[8] byte code parser should return an empty Vector if no instructions are passed") {
    assert(bcp.parse(Vector.empty) == Vector.empty)
  }
}
