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
        |  <Setter variable="a" function="20.0" activator="(b &gt; 10.0)" />
        |  <Setter variable="a" function="10.0" activator="(!(b &gt; 10.0))" />
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
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="((PitchRate &gt; 0.01) &amp; (Flaps &lt; 30.0))" />
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="((PitchRate &gt; 0.01) &amp; (!(Flaps &lt; 30.0)))" />
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="((!(PitchRate &gt; 0.01)) &amp; (PitchRate &lt; -0.01))" />
        |  <Setter variable="Flaps" function="Flaps" activator="((!(PitchRate &gt; 0.01)) &amp; (!(PitchRate &lt; -0.01)))" />
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
        |  <Setter variable="x" function="10.0" />
        |  <Setter variable="y" function="20.0" />
        |  <Setter variable="x" function="30.0" />
        |  <Setter variable="x" function="40.0" activator="(x = 30.0)" />
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = Variable("x")
        x := abs(25)
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" />
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        val x = Variable("x")
        for i <- 1 to 5 yield
          x := i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" />
        |  <Setter variable="x" function="2.0" />
        |  <Setter variable="x" function="3.0" />
        |  <Setter variable="x" function="4.0" />
        |  <Setter variable="x" function="5.0" />
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
        |  <Setter variable="Pitch" function="10.0" activator="(10.0 = 0.0)" />
        |  <Setter variable="Pitch" function="20.0" activator="(!(10.0 = 0.0))" />
        |  <Setter variable="Roll" function="1.0" activator="((10.0 = 0.0)?false:true)" />
        |  <Setter variable="Roll" function="20.0" activator="(!((10.0 = 0.0)?false:true))" />
        |</Variables>""")
    }
  }
