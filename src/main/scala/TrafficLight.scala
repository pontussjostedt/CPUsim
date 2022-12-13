package traffic
import java.awt.Graphics2D
final case class TrafficLight(red: Boolean, orange: Boolean, green: Boolean, name: String):
    def draw(x: Int, y: Int, width: Int = 100, height: Int = 300)(using g2d: Graphics2D): Unit =
        
        import java.awt.Color
        val colors: Vector[Color] = Vector(
            if red then Color(255, 0, 0) else Color(30, 0, 0),
            if orange then Color.ORANGE else Color(70, 50, 0),
            if green then Color(0, 255, 0) else Color(0, 30, 0)
        )
        
        val ovalX: Int = width / 2
        val ovalWidth: Int = (width * (2/3D)).toInt
        val ovalHeight: Int = (height * (1/3D)).toInt
        g2d.setColor(Color.GRAY)
        g2d.drawRect(x, y, width, height)
        for i <- colors.indices do
            g2d.setColor(colors(i))
            g2d.fillOval(x, y + i  * ovalHeight, width, ovalHeight)
        
        g2d.drawString(name, x, y + height + 30)
    
            

object TrafficLight:
    def pairFromDigit(control: Int): Vector[TrafficLight] =
        val desiredStringLength: Int = 6
        val binaryString = Integer.toBinaryString(control)
        val pins = binaryString
            .++:("0" * (desiredStringLength - binaryString.length())) //PAD
            .map(_ == '1')
            .reverse
            //println(s"BinaryString: $binaryString, ${pins.map(if _ then '1' else '0').mkString("")}")
        Vector(
            TrafficLight(pins(0), pins(1), pins(2), "huvudgata"),
            TrafficLight(pins(3), pins(4), pins(5), "sidogata"),  
        )
