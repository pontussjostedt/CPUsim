import computer.*
import traffic.TrafficLight
import sGui4s.SGui
import sGui4s.components.leafComponents.*
import sGui4s.components.containers.*
import sGui4s.events.*
import java.awt.GridLayout
import scala.Conversion
import sGui4s.events.Event


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
        case class Person(hello: String)
        
        val p = Some("hallå")
        p match
            case Some(value) => println(value)
            case _ =>
        
        val R0: RegIndex = 0
        val R1: RegIndex = 1
        given int2Int8: Conversion[Int, UInt8] = UInt8.noOverflow(_)
        given str2Int8: Conversion[String, UInt8] = str => UInt8.noOverflow(Integer.parseInt(str, 2))

        val lionCageProgram: Vector[Operation | (Address, Operation)] = Vector(
            "START" -> 
            OUT(R1),
            IN(R0),
            AND("1100", R0),
            SUB("1100", R0),

            BZ("MIDDLE-STATE", R0),
            B("START"),


            
            "MIDDLE-STATE" -> 
            IN(R0),
            SUB(4, R0),
            BZ("SUBTRACT", R0),
            SUB(4, R0),
            BZ("ADD", R0),
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
            IN(R0),
            AND(1, R0),
            BZ("SEQ", R0),
            LD("000100", R1),
            IN(R0),
            AND("10" , R0),
            BZ("START", R0),
            B("SEQ"),

            
            "SEQ" ->
            LD("001100", R1), CALL("SLEEP"),
            LD("001010", R1), CALL("SLEEP"),
            LD("001001", R1), CALL("SLEEP"),
            LD("011001", R1), CALL("SLEEP"),
            LD("100001", R1), CALL("SLEEP"),
            LD("010001", R1), CALL("SLEEP"),
            LD("001001", R1), CALL("SLEEP"),
            LD("001011", R1), CALL("SLEEP"),
            B("START"),

            "SLEEP" -> OUT(R1), LD(0, R0),
            "LOOP" -> ADD(1, R0),
            BZ("RETURN", R0),
            B("LOOP"),
            "RETURN" -> RET()
        )
        def display(registers: Vector[UInt8], pc: Int): Unit =
            val lights = TrafficLight.pairFromDigit(registers(1).value)
            //println(lights)
            window.render(g2d => for i <- lights.indices do lights(i).draw(0 + i * 200, 0)(using g2d))

        def getInput(registers: Vector[UInt8]): UInt8 = 
            def boolToBinary(b: Boolean): Char = if b then '1' else '0'
            println("Reading Input!!")
            val values = gui.getEventOrElseValues() match
                case (event: Event, values: Map[String, Any]) => values
                case values: Map[String, Any] => values
            val out: UInt8 = "0" * 6 + boolToBinary(values("Car").asInstanceOf[Boolean]) + boolToBinary(values("Day/Night").asInstanceOf[Boolean])
            println(out)
            out
            
            
            

        val cpu = CPU(stoplightProgram, 1000, getInput, display)
        

        
        
        
        cpu.run()



