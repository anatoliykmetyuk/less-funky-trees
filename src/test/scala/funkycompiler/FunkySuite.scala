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
        |  <Setter variable="a" function="20.0" activator="((!a_evaluationFlag1) & (b &gt; 10.0))/>
        |  <Setter variable="a_evaluationFlag1" function="true" activator="(b &gt; 10.0)/>
        |  <Setter variable="a_evaluationFlag1" function="true" activator="(!(b &gt; 10.0))/>
        |  <Setter variable="a" function="10.0" activator="((!a_evaluationFlag2) & (!(b &gt; 10.0)))/>
        |  <Setter variable="a_evaluationFlag2" function="true" activator="(!(b &gt; 10.0))/>
        |  <Setter variable="a_evaluationFlag2" function="true" activator="(b &gt; 10.0)/>
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
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="(((!Flaps_evaluationFlag3) & (Flaps &lt; 30.0)) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Flaps_evaluationFlag3" function="true" activator="((Flaps &lt; 30.0) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Flaps_evaluationFlag3" function="true" activator="((!(Flaps &lt; 30.0)) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="(((!Autotrim_evaluationFlag4) & (!(Flaps &lt; 30.0))) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Autotrim_evaluationFlag4" function="true" activator="((!(Flaps &lt; 30.0)) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Autotrim_evaluationFlag4" function="true" activator="((Flaps &lt; 30.0) & (PitchRate &gt; 0.01))/>
        |  <Setter variable="Flaps_evaluationFlag3" function="true" activator="(!(PitchRate &gt; 0.01))/>
        |  <Setter variable="Autotrim_evaluationFlag4" function="true" activator="(!(PitchRate &gt; 0.01))/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="(((!Flaps_evaluationFlag5) & (PitchRate &lt; -0.01)) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps_evaluationFlag5" function="true" activator="((PitchRate &lt; -0.01) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps_evaluationFlag5" function="true" activator="((!(PitchRate &lt; -0.01)) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps" function="Flaps" activator="(((!Flaps_evaluationFlag6) & (!(PitchRate &lt; -0.01))) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps_evaluationFlag6" function="true" activator="((!(PitchRate &lt; -0.01)) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps_evaluationFlag6" function="true" activator="((PitchRate &lt; -0.01) & (!(PitchRate &gt; 0.01)))/>
        |  <Setter variable="Flaps_evaluationFlag5" function="true" activator="(PitchRate &gt; 0.01)/>
        |  <Setter variable="Flaps_evaluationFlag6" function="true" activator="(PitchRate &gt; 0.01)/>
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
        |  <Setter variable="x" function="10.0" activator="(!x_evaluationFlag12)/>
        |  <Setter variable="x_evaluationFlag12" function="true" />
        |  <Setter variable="y" function="20.0" activator="(!y_evaluationFlag13)/>
        |  <Setter variable="y_evaluationFlag13" function="true" />
        |  <Setter variable="x" function="30.0" activator="(!x_evaluationFlag14)/>
        |  <Setter variable="x_evaluationFlag14" function="true" />
        |  <Setter variable="x" function="40.0" activator="((!x_evaluationFlag16) & (x = 30.0))/>
        |  <Setter variable="x_evaluationFlag16" function="true" activator="(x = 30.0)/>
        |  <Setter variable="x_evaluationFlag16" function="true" activator="(!(x = 30.0))/>
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = Variable("x")
        x := abs(25)
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" activator="(!x_evaluationFlag17)/>
        |  <Setter variable="x_evaluationFlag17" function="true" />
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        val x = Variable("x")
        for i <- 1 to 5 yield
          x := i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" activator="(!x_evaluationFlag18)/>
        |  <Setter variable="x_evaluationFlag18" function="true" />
        |  <Setter variable="x" function="2.0" activator="(!x_evaluationFlag19)/>
        |  <Setter variable="x_evaluationFlag19" function="true" />
        |  <Setter variable="x" function="3.0" activator="(!x_evaluationFlag20)/>
        |  <Setter variable="x_evaluationFlag20" function="true" />
        |  <Setter variable="x" function="4.0" activator="(!x_evaluationFlag21)/>
        |  <Setter variable="x_evaluationFlag21" function="true" />
        |  <Setter variable="x" function="5.0" activator="(!x_evaluationFlag22)/>
        |  <Setter variable="x_evaluationFlag22" function="true" />
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
        |  <Setter variable="Pitch" function="10.0" activator="((!Pitch_evaluationFlag24) & (10.0 = 0.0))/>
        |  <Setter variable="Pitch_evaluationFlag24" function="true" activator="(10.0 = 0.0)/>
        |  <Setter variable="Pitch_evaluationFlag24" function="true" activator="(!(10.0 = 0.0))/>
        |  <Setter variable="Pitch" function="20.0" activator="((!Pitch_evaluationFlag26) & (!(10.0 = 0.0)))/>
        |  <Setter variable="Pitch_evaluationFlag26" function="true" activator="(!(10.0 = 0.0))/>
        |  <Setter variable="Pitch_evaluationFlag26" function="true" activator="(10.0 = 0.0)/>
        |  <Setter variable="Roll" function="1.0" activator="((!Roll_evaluationFlag27) & ((10.0 = 0.0)?false:true))/>
        |  <Setter variable="Roll_evaluationFlag27" function="true" activator="((10.0 = 0.0)?false:true)/>
        |  <Setter variable="Roll_evaluationFlag27" function="true" activator="(!((10.0 = 0.0)?false:true))/>
        |  <Setter variable="Roll" function="20.0" activator="((!Roll_evaluationFlag28) & (!((10.0 = 0.0)?false:true)))/>
        |  <Setter variable="Roll_evaluationFlag28" function="true" activator="(!((10.0 = 0.0)?false:true))/>
        |  <Setter variable="Roll_evaluationFlag28" function="true" activator="((10.0 = 0.0)?false:true)/>
        |</Variables>""")
    }

    test("while loop") {
      funky {
        while Altitude <= 1000 do
          Pitch := 20
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="Pitch" function="20.0" activator="((!Pitch_evaluationFlag30) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="Pitch_evaluationFlag30" function="true" activator="(Altitude &lt;= 1000.0)/>
        |  <Setter variable="Pitch_evaluationFlag30" function="false" activator="(Altitude &lt;= 1000.0)/>
        |</Variables>""")
    }

    test("nested loop") {
      funky {
        while Altitude <= 1000 do
          val x = Variable("x")
          x := 0
          while Pitch <= 12345 do
            true
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="0.0" activator="((!x_evaluationFlag32) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="x_evaluationFlag32" function="true" activator="(Altitude &lt;= 1000.0)/>
        |  <Setter variable="x_evaluationFlag32" function="false" activator="((!(Pitch &lt;= 12345.0)) & (Altitude &lt;= 1000.0))/>
        |</Variables>""")
    }

    test("embedded nested loop") {
      funky {
        while Altitude <= 1000 do
          val x = Variable("x")
          x := 0
          while Pitch <= 12345 do
            true
          x := 50
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="0.0" activator="((!x_evaluationFlag36) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="x_evaluationFlag36" function="true" activator="(Altitude &lt;= 1000.0)/>
        |  <Setter variable="x" function="50.0" activator="(((!x_evaluationFlag38) & (!(Pitch &lt;= 12345.0))) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="x_evaluationFlag38" function="true" activator="((!(Pitch &lt;= 12345.0)) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="x_evaluationFlag36" function="false" activator="((!(Pitch &lt;= 12345.0)) & (Altitude &lt;= 1000.0))/>
        |  <Setter variable="x_evaluationFlag38" function="false" activator="((!(Pitch &lt;= 12345.0)) & (Altitude &lt;= 1000.0))/>
        |</Variables>""")
    }

    test("while nested in if") {
      funky {
        val x = Variable("x")
        x := 10
        if x > 5 then
          while x > 0 do
            x :-= 1
        x := 0
      }.compile ==> testStr("""
      """)
    }

    // test("if, while and sequential blocks interaction") {
    //   funky {
    //     val x = Variable("x")
    //     val y = Variable("y")
    //     x := 10
    //     if x > 5 then
    //       while x > 0 do
    //         x :-= 1
    //     if Pitch > 0.5 then x := 20 else x := 30

    //     while Pitch > 0 do
    //       y :+= 1

    //     if y > 5 then y := 0

    //     while Yaw > 0 do
    //       y :-= 1

    //     x := 0
    //   }.compile ==> testStr("""
    //   """)
    // }
  }
