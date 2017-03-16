package bc

import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import vm.VirtualMachine
import bc.ByteCodes._

/**
  * Testing ByteCodes
  */
class ByteCodeSpec extends FunSpec with Matchers with MockitoSugar with BeforeAndAfterEach {
  private val TopNum = 24
  private val BottomNum = 6
  private val Sum = TopNum + BottomNum
  private val Diff = TopNum - BottomNum
  private val Prod = TopNum * BottomNum
  private val Rem = TopNum / BottomNum
  private val Mod = TopNum % BottomNum
  private val vm1 = mock[VirtualMachine]
  private val vm2 = mock[VirtualMachine]
  private val vm3 = mock[VirtualMachine]
  private val vm4 = mock[VirtualMachine]

  describe("Mathematical ByteCodes") {
    it("IAdd must pop 2 values from the VirtualMachine and push the sum to a new VirtualMachine") {
      when(vm4.state).thenReturn(Vector(TopNum + BottomNum))
      IAdd().execute(vm1).state.head should be (Sum)
    }

    it("ISub must pop 2 values from the VirtualMachine and push the difference to a new VirtualMachine") {
      when(vm4.state).thenReturn(Vector(TopNum - BottomNum))
      ISub().execute(vm1).state.head should be (Diff)
    }

    it("IMul must pop 2 values from the VirtualMachine and push the product to a new VirtualMachine") {
      when(vm4.state).thenReturn(Vector(TopNum * BottomNum))
      IMul().execute(vm1).state.head should be (Prod)
    }

    it("IDiv must pop 2 values from the VirtualMachine and push the remainder to a new VirtualMachine") {
      when(vm4.state).thenReturn(Vector(TopNum / BottomNum))
      IDiv().execute(vm1).state.head should be (Rem)
    }

    it("IRem must pop 2 values from the VirtualMachine and push the modulus to a new VirtualMachine") {
      when(vm4.state).thenReturn(Vector(TopNum % BottomNum))
      IDiv().execute(vm1).state.head should be (Mod)
    }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    when(vm1.pop()).thenReturn((TopNum, vm2))
    when(vm2.pop()).thenReturn((BottomNum, vm3))
    when(vm3.push(ArgumentMatchers.anyInt())).thenReturn(vm4)
  }
}
