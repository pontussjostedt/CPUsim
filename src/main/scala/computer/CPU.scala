package computer





final case class CPU private (instructions: Vector[Operation], addressLUT: Map[Address, RegIndex], waitTimeNanos: Long, inputFunction: (Vector[UInt8]) => UInt8 , outputFunction: (Vector[UInt8], Int) => Unit):
    println(addressLUT)
    
    val stack = scala.collection.mutable.Stack.empty[Int]
    var register = Vector(UInt8(0), UInt8(0))
    var pc: Int = 0

    def run(): Unit =
        while instructions.indices.contains(pc) do
            val timeDone: Long = System.nanoTime() + waitTimeNanos
            execute(instructions(pc))
            while(timeDone >= System.nanoTime()) do ()

        System.out.println("OUT")

    def execute(operation: Operation): Unit =
        
        operation match
            case CALL(address) =>
                stack.push(pc + 1)
                pc = addressLUT(address)
            case RET() => pc = stack.pop()
            case BZ(address, regIndex) => if register(regIndex) == UInt8(0) then pc = addressLUT(address) else pc += 1
            case B(address) => pc = addressLUT(address)
            case LD(data, regIndex) => 
                register = register.updated(regIndex, data)
                pc+=1
            case ADD(data, regIndex) =>
                register = register.updated(regIndex, register(regIndex) + data)
                pc+=1
            case SUB(data, regIndex) =>
                register = register.updated(regIndex, register(regIndex) - data)
                pc+=1
            case AND(data, regIndex) =>
                register = register.updated(regIndex, register(regIndex) & data)
                pc+=1
            case IN(regIndex) => 
                register = register.updated(regIndex, inputFunction(register))
                pc += 1
            case OUT(regIndex) =>
                outputFunction(register, pc)
                pc += 1
            case DEBUG(message, regIndex) =>
                println(s"DEBUG: $message, register($regIndex) = ${register(regIndex)}")
                pc+=1
        
            
        
        

        


object CPU:
    def apply(program: Vector[Operation | (Address, Operation)], frequency: Float, inputFunction: (Vector[UInt8]) => UInt8, outputFunction: (Vector[UInt8], Int) => Unit): CPU =
        val addressReg = scala.collection.mutable.Map.empty[Address, RegIndex]
        val instructions = Array.ofDim[Operation](program.length);
        program.zipWithIndex.foreach {
            case (op: Operation, index: Int) => instructions(index) = op
            case ((addr: Address, op: Operation), index: Int) => {instructions(index) = op; addressReg += addr -> index}
        }
        new CPU(instructions.toVector, addressReg.toMap, (1/frequency * 1e9).toLong, inputFunction, outputFunction)