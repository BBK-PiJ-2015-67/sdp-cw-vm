package vm
import bc.ByteCode

/**
  * @see [[VirtualMachineParser]]
  */
object VirtualMachineParserImpl extends VirtualMachineParser {

  /**
    * @see [[VirtualMachineParser.parse()]]
    */
  override def parse(file: String): Vector[ByteCode] = ???

  /**
    * @see [[VirtualMachineParser.parseString()]]
    */
  override def parseString(str: String): Vector[ByteCode] = ???
}
