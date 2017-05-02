package vm
import bc.ByteCode

/**
  * [[VirtualMachine]] implementation
  * Given VirtualMachine is immutable, it is reasonable
  * to assume it is not a Singleton
  *
  * @see [[VirtualMachine]]
  */
case class VirtualMachineImpl(private val stack: Vector[Int] = Vector.empty) extends VirtualMachine {

  /**
    * @see [[VirtualMachine.execute()]]
    */
  override def execute(bc: Vector[ByteCode]): VirtualMachine =
    bc.foldLeft(this.asInstanceOf[VirtualMachine])((vm, code) => code.execute(vm))

  /**
    * @see [[VirtualMachine.executeOne()]]
    */
  override def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) =
    (bc.tail, bc.head.execute(this))

  /**
    * @see [[VirtualMachine.push()]]
    */
  override def push(value: Int): VirtualMachine = VirtualMachineImpl(value +: stack)

  /**
    * @see [[VirtualMachine.pop()]]
    * @throws MachineUnderflowException if the VM's state is empty
    */
  override def pop(): (Int, VirtualMachine) = {
    if (stack.isEmpty) throw new MachineUnderflowException("Virtual machine has an empty state")
    (stack.head, VirtualMachineImpl(stack.tail))
  }

  /**
    * @see [[VirtualMachine.state]]
    */
  override def state: Vector[Int] = stack
}
