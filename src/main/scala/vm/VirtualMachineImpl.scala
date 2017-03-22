package vm
import bc.ByteCode

/**
  * @see [[VirtualMachine]]
  */
case class VirtualMachineImpl() extends VirtualMachine {
  /**
    * @see [[VirtualMachine.execute()]]
    */
  override def execute(bc: Vector[ByteCode]): VirtualMachine = ???

  /**
    * @see [[VirtualMachine.executeOne()]]
    */
  override def executeOne(bc: Vector[ByteCode]): (Vector[ByteCode], VirtualMachine) = ???

  /**
    * @see [[VirtualMachine.push()]]
    */
  override def push(value: Int): VirtualMachine = ???

  /**
    * @see [[VirtualMachine.pop()]]
    */
  override def pop(): (Int, VirtualMachine) = ???

  /**
    * @see [[VirtualMachine.state]]
    */
  override def state: Vector[Int] = ???
}
