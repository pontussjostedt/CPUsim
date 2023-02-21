package computer

final case class UInt8 private (value: Int):
    infix def +(other: UInt8): UInt8 = UInt8(value + other.value)
    infix def -(other: UInt8): UInt8 = UInt8(value - other.value)
    infix def &(other: UInt8): UInt8 = UInt8(value & other.value)

  

    override def canEqual(that: Any): Boolean = that.isInstanceOf[UInt8] || that.isInstanceOf[Int]
    override def equals(other: Any): Boolean =
        other match
            case UInt8(otherValue) => value == otherValue
            case otherValue: Int => value == otherValue
            case _ => false
        
            
        
    override def hashCode(): Int = 
        value.hashCode()

object UInt8:
    val validRange: Range = 0 to 255
    def apply(value: Int): UInt8 = new UInt8((value + 256) % 256)
    def noOverflow(value: Int): UInt8 =
        assert(validRange.contains(value), "IntegerOverflow")
        new UInt8(value)