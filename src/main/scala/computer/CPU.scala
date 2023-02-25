package computer

import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter




/**
  * 
  *
  * @param instructions Array of operations to be executed, representing the program memory of the CPU
  * @param addressLUT A map readable line name -> lineindex
  * @param waitTimeNanos min time between operations
  * @param inputFunction used to get input when calling IN
  * @param outputFunction used to display out
  */
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
    /**
      * Constructor for CPU
      * @return CPU loaded with program
      */
    def apply(program: Vector[Operation | (Address, Operation)], frequency: Float, inputFunction: (Vector[UInt8]) => UInt8, outputFunction: (Vector[UInt8], Int) => Unit): CPU =
        val addressReg = scala.collection.mutable.Map.empty[Address, RegIndex]
        val instructions = Array.ofDim[Operation](program.length);
        program.zipWithIndex.foreach {
            case (op: Operation, index: Int) => instructions(index) = op
            case ((addr: Address, op: Operation), index: Int) => {instructions(index) = op; addressReg += addr -> index}
        }
        dumpCode(instructions, addressReg.toMap)
        new CPU(instructions.toVector, addressReg.toMap, (1/frequency * 1e9).toLong, inputFunction, outputFunction)

    /**
     *  Dumps code into a file which can then be loaded onto the FPGA cpu
     * this method is a mess :)
    */
    def dumpCode(operations: Array[Operation], addresses: Map[Address, Int]): Unit = 
        def empty = "0" * 9
        val file = new File("out.hex")
        val bw = new BufferedWriter(new FileWriter(file))
        def d(p: Int) = Integer.toBinaryString(p)
       
        def uint2str(v: UInt8): String = 
            val out = Integer.toBinaryString(v.value)
            "0" * (8 - out.length()) + out
        operations.map {
                case DEBUG(message, regIndex) => throw Exception("Real world CPU does not support Debug")
                case RET() => "0010" + empty -> "Ret"
                case IN(regIndex) => "1000" + d(regIndex) + "0" * 8 -> "In"
                case SUB(data, regIndex) => "0110" + d(regIndex) + uint2str(data) -> "Sub"
                case LD(data, regIndex) => "0111" + d(regIndex) + uint2str(data)-> "LD"
                case AND(data, regIndex) => "1010" + d(regIndex) + uint2str(data) -> "AND"
                case CALL(address) => "0001" + "0" + uint2str(UInt8(addresses(address))) -> "Call"
                case B(address) => "0100" + "0" + uint2str(UInt8(addresses(address))) -> "B"
                case BZ(address, regIndex) => "0011" + d(regIndex) + uint2str(UInt8(addresses(address))) -> "BZ"
                case ADD(data, regIndex) => "0101" + d(regIndex) + uint2str(data) -> "ADD"
                case OUT(regIndex) => "1001" + d(regIndex) + "0" * 8 -> "OUT"
        }.foreach {case (code, description) =>
            val out = Integer.toHexString(Integer.parseInt(code, 2))
            bw.write("0" *(4 - out.length()) + out + ";" + description + "\n")
        }

    
        bw.close()
        