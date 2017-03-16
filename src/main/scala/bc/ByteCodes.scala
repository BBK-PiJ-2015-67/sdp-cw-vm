package bc
import vm.VirtualMachine

/**
  * @author lmignot
  */
package object ByteCodes {

  /**
    * Math ByteCodes all pop the top 2 values off the VirtualMachine stack
    * and perform some mathematical operation, pushing the result
    *
    * @param f some arithmetic op to perform on 2 integer values
    */
  abstract class MathByteCode(f: (Int, Int) => Int) extends ByteCode {
    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = {
      val (x, vmNext) = vm.pop()
      val (y, vmFinal) = vmNext.pop()
      vmFinal.push(f(x, y))
    }
  }

  /**
    * Manipulation ByteCodes take the top value from the [[VirtualMachine]]
    * and push the manipulated value
    *
    * @param f some manipulation to perform on a single integer
    */
  abstract class ManipulateByteCode(f: (Int) => Int) extends ByteCode {
    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = {
      val (x, vmNext) = vm.pop()
      vmNext.push(f(x))
    }
  }

  /**
    * Pushes the integer value onto the [[VirtualMachine]] stack
    *
    * @see [[ByteCode]]
    */
  case class IConst(num: Int) extends ByteCode {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("iconst")

    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = vm.push(num)
  }

  /**
    * Push the sum of 2 Int values onto the stack
    *
    * @see [[MathByteCode]]
    */
  case class IAdd() extends MathByteCode(_ + _) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("iadd")
  }

  /**
    * Push the difference of 2 Int values onto the stack
    *
    * @see [[MathByteCode]]
    */
  case class ISub() extends MathByteCode(_ - _) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("isub")
  }

  /**
    * Push the product of 2 Int values onto the stack
    *
    * @see [[MathByteCode]]
    */
  case class IMul() extends MathByteCode(_ * _) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("imul")
  }

  /**
    * Push the remainder of 2 Int values onto the stack
    *
    * @see [[MathByteCode]]
    */
  case class IDiv() extends MathByteCode(_ / _) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("idiv")
  }

  /**
    * Push the modulus of 2 Int values onto the stack
    *
    * @see [[MathByteCode]]
    */
  case class IRem() extends MathByteCode(_ % _) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("irem")
  }

  /**
    * Negate the top value of a [[VirtualMachine]]
    *
    * @see [[ManipulateByteCode]]
    */
  case class INeg() extends ManipulateByteCode(-_) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("ineg")
  }

  /**
    * Increment the top value of a [[VirtualMachine]]
    *
    * @see [[ManipulateByteCode]]
    */
  case class IInc() extends ManipulateByteCode(_ + 1) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("iinc")
  }

  /**
    * Decrement the top value of a [[VirtualMachine]]
    *
    * @see [[ManipulateByteCode]]
    */
  case class IDec() extends ManipulateByteCode(_ - 1) {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("idec")
  }

  /**
    * Swaps the top 2 values of the [[VirtualMachine]] stack
    *
    * @see [[ByteCode]]
    */
  case class ISwap() extends ByteCode {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("iswap")

    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = {
      val (first, vmNext) = vm.pop()
      val (second, vmMid) = vmNext.pop()
      vmMid.push(first).push(second)
    }
  }

  /**
    * Swaps the top 2 values of the [[VirtualMachine]] stack
    *
    * @see [[ByteCode]]
    */
  case class IDup() extends ByteCode {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("idup")

    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = {
      val (first, vmNext) = vm.pop()
      vmNext.push(first).push(first)
    }
  }

  /**
    * Print the top value of the [[VirtualMachine]] stack
    *
    * @see [[ByteCode]]
    */
  case class Print() extends ByteCode {
    /**
      * @see [[ByteCode.code]]
      */
    override val code: Byte = bytecode("print")

    /**
      * @see [[ByteCode.execute]]
      */
    override def execute(vm: VirtualMachine): VirtualMachine = {
      val (value, outVm) = vm.pop()
      println(value)
      outVm
    }
  }

}
