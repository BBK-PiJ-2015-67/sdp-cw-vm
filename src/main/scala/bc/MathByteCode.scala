package bc
import vm.VirtualMachine

/**
  * Math ByteCodes all pop the top 2 values off the VirtualMachine stack
  * and perform some mathematical operation, pushing the result
  *
  * @param f function that performs some arithmetic op on 2 Int values
  *          and returns the result
  */
abstract class MathByteCode(f: (Int, Int) => Int) extends ByteCode {
  /**
    * @return a new [[VirtualMachine]] with the result of executing
    *         the math op pushed to the top of the VM stack
    * @see [[bc.ByteCode.code]]
    */
  override def execute(vm: VirtualMachine): VirtualMachine = {
    val (x, vmNext) = vm.pop()
    val (y, _) = vm.pop()
    vmNext.push(f(x, y))
  }
}
