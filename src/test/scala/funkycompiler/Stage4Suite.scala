package funkycompiler

import utest._
import testPrograms.*


object Stage4Suite extends TestSuite:
  def testStr(str: String) = str.stripMargin.drop(1)

  val tests = Tests {
    test("if statement"){
      stage4.mkXml(stage3.mkVarDefs(stage(ifStatement))).toString ==> testStr("""
        |<Variables>
        |  <Setter variable="a" function="20.0" activator="(b &gt; 10.0)" />
        |  <Setter variable="a" function="10.0" activator="(!(b &gt; 10.0))" />
        |</Variables>""")
    }

    test("nested if statement") {
      stage4.mkXml(stage3.mkVarDefs(stage(nestedIfStatement))).toString ==> testStr("""
        |<Variables>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="((PitchRate &gt; 0.01) &amp; (Flaps &lt; 30.0))" />
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="((PitchRate &gt; 0.01) &amp; (!(Flaps &lt; 30.0)))" />
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="((!(PitchRate &gt; 0.01)) &amp; (PitchRate &lt; -0.01))" />
        |  <Setter variable="Flaps" function="Flaps" activator="((!(PitchRate &gt; 0.01)) &amp; (!(PitchRate &lt; -0.01)))" />
        |</Variables>""")
    }

    test("blocks should accumulate assignments and if statements") {
      stage4.mkXml(stage3.mkVarDefs(stage(blocksAccumulation))).toString ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="10.0" />
        |  <Setter variable="y" function="20.0" />
        |  <Setter variable="x" function="30.0" />
        |  <Setter variable="x" function="40.0" activator="(x = 30.0)" />
        |</Variables>""")
    }

    test("calls") {
      stage4.mkXml(stage3.mkVarDefs(stage(calls))).toString ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" />
        |</Variables>""")
    }

    test("loop unrolling") {
      stage4.mkXml(stage3.mkVarDefs(stage(loopUnrolling))).toString ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" />
        |  <Setter variable="x" function="2.0" />
        |  <Setter variable="x" function="3.0" />
        |  <Setter variable="x" function="4.0" />
        |  <Setter variable="x" function="5.0" />
        |</Variables>""")
    }
  }
