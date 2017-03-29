package bc

import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import vm.VirtualMachine
import bc.byteCodes._

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
  private val Neg = -TopNum
  private val Inc = TopNum + 1
  private val Dec = TopNum - 1
  private val Zero = 0
  private val vm1 = mock[VirtualMachine]
  private val vm2 = mock[VirtualMachine]
  private val vm3 = mock[VirtualMachine]
  private val vm4 = mock[VirtualMachine]
  private val vm5 = mock[VirtualMachine]

  describe("Mathematical ByteCodes") {
    it("IAdd must return VM whose top value is sum of top 2 values of a VM") {
      when(vm4.state).thenReturn(Vector(TopNum + BottomNum))
      IAdd().execute(vm1).state.head should be (Sum)
    }

    it("ISub must return VM whose top value is difference of top 2 values of a VM") {
      when(vm4.state).thenReturn(Vector(TopNum - BottomNum))
      ISub().execute(vm1).state.head should be (Diff)
    }

    it("IMul must return VM whose top value is product of top 2 values of a VM") {
      when(vm4.state).thenReturn(Vector(TopNum * BottomNum))
      IMul().execute(vm1).state.head should be (Prod)
    }

    it("IDiv must return VM whose top value is quotient of top 2 values of a VM") {
      when(vm4.state).thenReturn(Vector(TopNum / BottomNum))
      IDiv().execute(vm1).state.head should be (Rem)
    }

    it("IDiv should throw an arithmetic exception if 2nd value of original VM is Zero") {
      when(vm2.pop()).thenReturn((Zero, vm3))
      intercept[ArithmeticException] {
        IDiv().execute(vm1)
      }
    }

    it("IRem must return a VM whose top value is modulus of top 2 values of a VM") {
      when(vm4.state).thenReturn(Vector(TopNum % BottomNum))
      IDiv().execute(vm1).state.head should be (Mod)
    }
  }

  describe("manipulating ByteCodes") {
    it("INeg must return a VM whose top value is the negation of the original VM") {
      when(vm3.state).thenReturn(Vector(-TopNum))
      INeg().execute(vm1).state.head should be (Neg)
    }

    it("IInc must return a VM whose top value has been incremented") {
      when(vm3.state).thenReturn(Vector(TopNum + 1))
      IInc().execute(vm1).state.head should be (Inc)
    }

    it("IDec must return a VM whose top value has been decremented") {
      when(vm3.state).thenReturn(Vector(TopNum - 1))
      IDec().execute(vm1).state.head should be (Dec)
    }
  }

  describe("other ByteCodes") {
    it("ISwap must return a VM whose top 2 values have been swapped") {
      when(vm5.state).thenReturn(Vector(BottomNum, TopNum))
      val state = ISwap().execute(vm1).state
      state.head should be (BottomNum)
      state.tail.head should be (TopNum)
    }

    it("IDup must return a VM whose top value has been duplicated") {
      when(vm4.state).thenReturn(Vector(TopNum, TopNum))
      val state = IDup().execute(vm1).state
      state.head should be (TopNum)
      state.tail.head should be (TopNum)
    }

    it("Print must print top value of original VM and return VM with top value removed") {
      when(vm2.state).thenReturn(Vector(BottomNum))
      Print().execute(vm1).state.head should be (BottomNum)
      // todo: test stdout somehow
    }

  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    when(vm1.pop()).thenReturn((TopNum, vm2))
    when(vm2.pop()).thenReturn((BottomNum, vm3))
    when(vm2.push(ArgumentMatchers.anyInt())).thenReturn(vm3)
    when(vm3.push(ArgumentMatchers.anyInt())).thenReturn(vm4)
    when(vm4.push(ArgumentMatchers.anyInt())).thenReturn(vm5)
  }
}
