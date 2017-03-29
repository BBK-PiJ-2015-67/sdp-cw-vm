package vm

import factory.VirtualMachineFactory
import org.scalatest.FunSuite

class PublicVirtualMachineSuite extends FunSuite {
  val TestInt = 99

  val vmp = VirtualMachineFactory.virtualMachineParser
  val bcp = VirtualMachineFactory.byteCodeParser
  val vm  = VirtualMachineFactory.virtualMachine

  test("[16] vm.push should work") {
    val vm2 = vm.push(TestInt)
    assert(vm2.state.head == TestInt)
    assert(vm2 ne vm)
  }

  test("[17] vm.pop should work") {
    val vm2 = vm.push(TestInt)
    val (top, vm3) = vm2.pop()
    assert(vm2.state.head == TestInt)
    assert(vm3.state.isEmpty)
    assert(top == TestInt)
    assert(vm2 ne vm)
    assert(vm3 ne vm2)
    assert(vm3 ne vm)
  }

  test("[18] vm.pop should throw if empty") {
    intercept[MachineUnderflowException] {
      vm.pop()
    }
  }

  test("[19] a virtual machine should execute a program") {
    val bc  = vmp.parse("programs/p05.vm")
    val vm2 = vm.execute(bc)
  }

  test("[20] iconst should work correctly") {
    val bc  = vmp.parseString("iconst 1")
    val (bc2, vm2) = vm.executeOne(bc)
    assert(vm2.state.head == 1)
  }

  test("[21] iadd should work correctly") {
    val bc  = vmp.parseString("iconst 1\niconst 2\niadd")
    var next = vm.executeOne(bc)
    assert(next._2.state.head == 1)
    next = next._2.executeOne(next._1)
    assert(next._2.state.head == 2)
    next = next._2.executeOne(next._1)
    assert(next._2.state.head == 3)
  }

  test("[22] isub should work correctly") {
    val bc  = vmp.parseString("iconst 1\niconst 2\nisub")
    var next = vm.executeOne(bc)
    assert(next._2.state.head == 1)
    next = next._2.executeOne(next._1)
    assert(next._2.state.head == 2)
    next = next._2.executeOne(next._1)
    assert(next._2.state.head == 1)
  }

  test("[23] iswap should work correctly") {
    val bc  = vmp.parseString("iconst 1\niconst 2\niswap")
    var next = vm.executeOne(bc)
    assert(next._2.state.head == 1)
    next = next._2.executeOne(next._1)
    assert(next._2.state.head == 2)
    next = next._2.executeOne(next._1)
    assert(next._2.state(0) == 1)
    assert(next._2.state(1) == 2)
  }
}
