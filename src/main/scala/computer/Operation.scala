package computer

sealed trait Operation
type RegIndex = Int
type Address = String
case class CALL(address: Address) extends Operation
case class RET() extends Operation
case class BZ(address: Address, regIndex: RegIndex) extends Operation
case class B(address: Address) extends Operation


case class LD(data: UInt8, regIndex: RegIndex) extends Operation
case class ADD(data: UInt8, regIndex: RegIndex) extends Operation
case class SUB(data: UInt8, regIndex: RegIndex) extends Operation

case class IN(regIndex: RegIndex) extends Operation
case class OUT(regIndex: RegIndex) extends Operation
   

