import computer.*
import traffic.TrafficLight
import sGui4s.SGui
import sGui4s.components.leafComponents.*
import sGui4s.components.containers.*
import sGui4s.events.*
import java.awt.GridLayout
import scala.Conversion


object Foo:
    val window = Window("TrafficThing", 512, 512)
     val gui = SGui(
            SFrame("ControlPanel", GridLayout(2, 1))(
                SRadioButton("Day/Night"),
                SRadioButton("Car"),
            )
        )
    @main
    def bob(): Unit =
        val R0: RegIndex = 0
        val R1: RegIndex = 1
        given int2Int8: Conversion[Int, UInt8] = UInt8.noOverflow(_)
        given str2Int8: Conversion[String, UInt8] = str => UInt8.noOverflow(Integer.parseInt(str, 2))

        val lionCageProgram: Vector[Operation | (Address, Operation)] = Vector(
            "START" -> 
            OUT(R1),
            IN(R0),
            SUB(12, R0),
            BZ("MIDDLE-STATE", R0),
            B("START"),


            
            "MIDDLE-STATE" -> 
            IN(R0),
            SUB(4, R0),
            BZ("ADD", R0),
            SUB(4, R0),
            BZ("SUBTRACT", R0),
            B("MIDDLE-STATE"),

            
            "SUBTRACT" ->
            SUB(1, R1),
            B("START"),
            "ADD" ->
            ADD(1, R1),
            B("START"),
        )

        val stoplightProgram: Vector[Operation | (Address, Operation)] = Vector(
            "START" ->
            OUT(R1),
            CALL("SEQ"),
            B("START"),

            "SEQ" ->
            LD("001100", 1), CALL("SLEEP"),
            LD("001010", 1), CALL("SLEEP"),
            LD("001001", 1), CALL("SLEEP"),
            LD("011001", 1), CALL("SLEEP"),
            LD("100001", 1), CALL("SLEEP"),
            LD("010001", 1), CALL("SLEEP"),
            LD("001001", 1), CALL("SLEEP"),
            LD("001011", 1), CALL("SLEEP"),
            RET(),

            "SLEEP" -> OUT(R1), LD(0, R0),
            "LOOP" -> ADD(1, R0),
            BZ("RETURN", R0),
            B("LOOP"),
            "RETURN" -> RET()
        )
        def display(registers: Vector[UInt8], pc: Int): Unit =
            val lights = TrafficLight.pairFromDigit(registers(1).value)
            println(lights)
            window.render(g2d => for i <- lights.indices do lights(i).draw(0 + i * 200, 0)(using g2d))

        def getInput(registers: Vector[UInt8]): UInt8 = 
            given str2Bool: Conversion[Boolean, String] = if _ then "1" else "0"
            val (event, values) = gui.awaitEvent()
            "000000" + values("Car") + values("Day/Night")
            
            
            

        val cpu = CPU(stoplightProgram, 1000, (reg) => UInt8(4), display)
        

        
        
        
        cpu.run()


