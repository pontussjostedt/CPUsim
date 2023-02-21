<h1>CPUSimulator</h1>
Program used to simulate code for mini CPU built on an FPGA.


<h2>Example program</h2>
```scala
val exampleProgram: Vector[Operation | (Address, Operation)] = Vector(
            "START" -> 
            OUT(R1), //Load R1 into output
            IN(R0),  // write input to R0
            AND("1100", R0), //bitmask
            SUB("1100", R0), //Subtract 1100 from R0

            BZ("MIDDLE-STATE", R0), //If R0 == 0 goto "MIDDLE-STATE"
            B("START"), //goto "START"


            
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
```

see main for more details
<h2>How to use</h2>
1. clone repo
2. make sure you have sbt and scala 3.0 or later installed on your machine
3. sbt run

<h2> Dependency </h2>
sgui4s-alpha.jar - https://github.com/pontussjostedt/scalaSimpleGUI
Quick minimal swing GUI generator