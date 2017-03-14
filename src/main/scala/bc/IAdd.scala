package bc
import vm.VirtualMachine

/**
  * @author lmignot
  */
case class IAdd() extends ByteCode {
  /**
    * @see [[bc.ByteCode.code]]
    */
  override val code: Byte = bytecode("iadd")

  /**
    * @see [[bc.ByteCode.execute()]]
    */
  override def execute(vm: VirtualMachine): VirtualMachine = {
    val (x, vm2) = vm.pop()
    val (y, _) = vm2.pop()
    vm2.push(x + y)
  }
}
