package funkycompiler

import utest.*
import stage3.{ Variable, Tree }
import stdlib.*


object FunkySuite extends TestSuite:
  def testStr(str: String) = str.stripMargin.drop(1)

  val tests = Tests {
    test("if statement") {
      funky {
        val a = Variable("a")
        val b = Variable("b")
        if b > 10 then a := 20 else a := 10
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="a" function="20.0" activator="((((!a_evaluationFlag1) & true) & (b &gt; 10.0)) & (true & true))/>
        |  <Setter variable="a_evaluationFlag1" function="true" activator="(((true & true) & (b &gt; 10.0)) & (true & true))/>
        |  <Setter variable="a" function="10.0" activator="((((!a_evaluationFlag2) & true) & (!(b &gt; 10.0))) & (true & true))/>
        |  <Setter variable="a_evaluationFlag2" function="true" activator="(((true & true) & (!(b &gt; 10.0))) & (true & true))/>
        |</Variables>""")
    }

    test("nested if statement") {
      funky {
        val Flaps = Variable("Flaps")
        val Autotrim = Variable("Autotrim")

        val maxPitchRate = 0.01
        object flaps:
          val extensionRate = 0.02
          val maxExtension = 30

        if PitchRate > maxPitchRate then
          if Flaps < flaps.maxExtension then
            Flaps := Flaps + flaps.extensionRate
          else
            Autotrim := Autotrim + flaps.extensionRate
        else if PitchRate < -maxPitchRate then
          Flaps := Flaps - flaps.extensionRate
        else Flaps := Flaps
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="((((((!Flaps_evaluationFlag3) & (true & true)) & (Flaps &lt; 30.0)) & (true & true)) & (PitchRate &gt; 0.01)) & (true & true))/>
        |  <Setter variable="Flaps_evaluationFlag3" function="true" activator="(((((true & (true & true)) & (Flaps &lt; 30.0)) & (true & true)) & (PitchRate &gt; 0.01)) & (true & true))/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="((((((!Autotrim_evaluationFlag4) & (true & true)) & (!(Flaps &lt; 30.0))) & (true & true)) & (PitchRate &gt; 0.01)) & (true & true))/>
        |  <Setter variable="Autotrim_evaluationFlag4" function="true" activator="(((((true & (true & true)) & (!(Flaps &lt; 30.0))) & (true & true)) & (PitchRate &gt; 0.01)) & (true & true))/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="((((((!Flaps_evaluationFlag5) & (true & true)) & (PitchRate &lt; -0.01)) & (true & true)) & (!(PitchRate &gt; 0.01))) & (true & true))/>
        |  <Setter variable="Flaps_evaluationFlag5" function="true" activator="(((((true & (true & true)) & (PitchRate &lt; -0.01)) & (true & true)) & (!(PitchRate &gt; 0.01))) & (true & true))/>
        |  <Setter variable="Flaps" function="Flaps" activator="((((((!Flaps_evaluationFlag6) & true) & (!(PitchRate &lt; -0.01))) & (true & true)) & (!(PitchRate &gt; 0.01))) & (true & true))/>
        |  <Setter variable="Flaps_evaluationFlag6" function="true" activator="(((((true & true) & (!(PitchRate &lt; -0.01))) & (true & true)) & (!(PitchRate &gt; 0.01))) & (true & true))/>
        |</Variables>""")
    }

    test("blocks should accumulate assignments and if statements") {
      funky {
        val x = Variable("x")
        val y = Variable("y")
        x := 10
        y := 20
        x := 30
        if x === 30 then
          x := 40
        50
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="10.0" activator="((!x_evaluationFlag12) & true)/>
        |  <Setter variable="x_evaluationFlag12" function="true" activator="(true & true)/>
        |  <Setter variable="y" function="20.0" activator="(((!y_evaluationFlag13) & true) & true)/>
        |  <Setter variable="y_evaluationFlag13" function="true" activator="((true & true) & true)/>
        |  <Setter variable="x" function="30.0" activator="(((!x_evaluationFlag14) & true) & true)/>
        |  <Setter variable="x_evaluationFlag14" function="true" activator="((true & true) & true)/>
        |  <Setter variable="x" function="40.0" activator="(((((!x_evaluationFlag16) & true) & (x = 30.0)) & (true & true)) & true)/>
        |  <Setter variable="x_evaluationFlag16" function="true" activator="((((true & true) & (x = 30.0)) & (true & true)) & true)/>
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = Variable("x")
        x := abs(25)
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" activator="((!x_evaluationFlag17) & (true & true))/>
        |  <Setter variable="x_evaluationFlag17" function="true" activator="(true & (true & true))/>
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        val x = Variable("x")
        for i <- 1 to 5 yield
          x := i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" activator="((!x_evaluationFlag18) & true)/>
        |  <Setter variable="x_evaluationFlag18" function="true" activator="(true & true)/>
        |  <Setter variable="x" function="2.0" activator="(((!x_evaluationFlag19) & true) & true)/>
        |  <Setter variable="x_evaluationFlag19" function="true" activator="((true & true) & true)/>
        |  <Setter variable="x" function="3.0" activator="(((!x_evaluationFlag20) & true) & true)/>
        |  <Setter variable="x_evaluationFlag20" function="true" activator="((true & true) & true)/>
        |  <Setter variable="x" function="4.0" activator="(((!x_evaluationFlag21) & true) & true)/>
        |  <Setter variable="x_evaluationFlag21" function="true" activator="((true & true) & true)/>
        |  <Setter variable="x" function="5.0" activator="(((!x_evaluationFlag22) & true) & true)/>
        |  <Setter variable="x_evaluationFlag22" function="true" activator="((true & true) & true)/>
        |</Variables>""")
    }

    test("boolean function") {
      def f(x: Tree): Tree = funky {
        if x === 0 then
          Pitch := 10
          false
        else
          Pitch := 20
          true
      }

      funky {
        if f(10) then
          Roll := 1
        else
          Roll := 20
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="Pitch" function="10.0" activator="((((!Pitch_evaluationFlag24) & true) & (10.0 = 0.0)) & (true & true))/>
        |  <Setter variable="Pitch_evaluationFlag24" function="true" activator="(((true & true) & (10.0 = 0.0)) & (true & true))/>
        |  <Setter variable="Pitch" function="20.0" activator="((((!Pitch_evaluationFlag26) & true) & (!(10.0 = 0.0))) & (true & true))/>
        |  <Setter variable="Pitch_evaluationFlag26" function="true" activator="(((true & true) & (!(10.0 = 0.0))) & (true & true))/>
        |  <Setter variable="Roll" function="1.0" activator="((((!Roll_evaluationFlag27) & true) & ((10.0 = 0.0)?false:true)) & (true | true))/>
        |  <Setter variable="Roll_evaluationFlag27" function="true" activator="(((true & true) & ((10.0 = 0.0)?false:true)) & (true | true))/>
        |  <Setter variable="Roll" function="20.0" activator="((((!Roll_evaluationFlag28) & true) & (!((10.0 = 0.0)?false:true))) & (true | true))/>
        |  <Setter variable="Roll_evaluationFlag28" function="true" activator="(((true & true) & (!((10.0 = 0.0)?false:true))) & (true | true))/>
        |</Variables>""")
    }
  }
