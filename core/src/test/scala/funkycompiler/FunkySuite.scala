package funkycompiler

import utest.*
import stage3.{ Variable, Tree }
import stdlib.*


object FunkySuite extends TestSuite:
  def testStr(str: String) = str.stripMargin.drop(1)

  override def utestBeforeEach(path: Seq[String]): Unit =
    resetFreshVarCounter()

  val tests = Tests {
    test("if statement") {
      funky {
        val a = Variable("a")
        val b = Variable("b")
        if b > 10 then a := 20 else a := 10
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="if_condition_3" function="(b &gt; 10.0)" activator="(!evaluationFlag_if_condition_3_4)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_4" function="true" />
        |  <Setter variable="a" function="20.0" activator="((!evaluationFlag_a_1) & if_condition_3)"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="if_condition_3"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="a" function="10.0" activator="((!evaluationFlag_a_2) & (!if_condition_3))"/>
        |  <Setter variable="evaluationFlag_a_2" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="evaluationFlag_a_2" function="true" activator="if_condition_3"/>
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
        |  <Setter variable="if_condition_7" function="(PitchRate &gt; 0.01)" activator="(!evaluationFlag_if_condition_7_10)"/>
        |  <Setter variable="evaluationFlag_if_condition_7_10" function="true" />
        |  <Setter variable="if_condition_3" function="(Flaps &lt; 30.0)" activator="((!evaluationFlag_if_condition_3_8) & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="if_condition_7"/>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="(((!evaluationFlag_Flaps_1) & if_condition_3) & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(if_condition_3 & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="((!if_condition_3) & if_condition_7)"/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="(((!evaluationFlag_Autotrim_2) & (!if_condition_3)) & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="((!if_condition_3) & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(if_condition_3 & if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="if_condition_6" function="(PitchRate &lt; -0.01)" activator="((!evaluationFlag_if_condition_6_9) & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_9" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="(((!evaluationFlag_Flaps_4) & if_condition_6) & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="(if_condition_6 & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="((!if_condition_6) & (!if_condition_7))"/>
        |  <Setter variable="Flaps" function="Flaps" activator="(((!evaluationFlag_Flaps_5) & (!if_condition_6)) & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="((!if_condition_6) & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="(if_condition_6 & (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_9" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="if_condition_7"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="if_condition_7"/>
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
        |  <Setter variable="x" function="10.0" activator="(!evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_7" function="true" />
        |  <Setter variable="y" function="20.0" activator="(!evaluationFlag_y_8)"/>
        |  <Setter variable="evaluationFlag_y_8" function="true" />
        |  <Setter variable="x" function="30.0" activator="(!evaluationFlag_x_9)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" />
        |  <Setter variable="if_condition_12" function="(x = 30.0)" activator="(!evaluationFlag_if_condition_12_13)"/>
        |  <Setter variable="evaluationFlag_if_condition_12_13" function="true" />
        |  <Setter variable="x" function="40.0" activator="((!evaluationFlag_x_11) & if_condition_12)"/>
        |  <Setter variable="evaluationFlag_x_11" function="true" activator="if_condition_12"/>
        |  <Setter variable="evaluationFlag_x_11" function="true" activator="(!if_condition_12)"/>
        |</Variables>""")
    }

    test("calls") {
      funky {
        val x = Variable("x")
        x := abs(25)
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="abs(25.0)" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |</Variables>""")
    }

    test("loop unrolling") {
      funky {
        val x = Variable("x")
        for i <- 1 to 5 yield
          x := i
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="x" function="1.0" activator="(!evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_1" function="true" />
        |  <Setter variable="x" function="2.0" activator="(!evaluationFlag_x_2)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" />
        |  <Setter variable="x" function="3.0" activator="(!evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" />
        |  <Setter variable="x" function="4.0" activator="(!evaluationFlag_x_4)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" />
        |  <Setter variable="x" function="5.0" activator="(!evaluationFlag_x_5)"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" />
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
        |  <Setter variable="if_condition_5" function="(10.0 = 0.0)" activator="(!evaluationFlag_if_condition_5_10)"/>
        |  <Setter variable="evaluationFlag_if_condition_5_10" function="true" />
        |  <Setter variable="Pitch" function="10.0" activator="((!evaluationFlag_Pitch_2) & if_condition_5)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="if_condition_5"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="(!if_condition_5)"/>
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_4) & (!if_condition_5))"/>
        |  <Setter variable="evaluationFlag_Pitch_4" function="true" activator="(!if_condition_5)"/>
        |  <Setter variable="evaluationFlag_Pitch_4" function="true" activator="if_condition_5"/>
        |  <Setter variable="if_condition_8" function="((10.0 = 0.0)?false:true)" activator="(!evaluationFlag_if_condition_8_9)"/>
        |  <Setter variable="evaluationFlag_if_condition_8_9" function="true" />
        |  <Setter variable="Roll" function="1.0" activator="((!evaluationFlag_Roll_6) & if_condition_8)"/>
        |  <Setter variable="evaluationFlag_Roll_6" function="true" activator="if_condition_8"/>
        |  <Setter variable="evaluationFlag_Roll_6" function="true" activator="(!if_condition_8)"/>
        |  <Setter variable="Roll" function="20.0" activator="((!evaluationFlag_Roll_7) & (!if_condition_8))"/>
        |  <Setter variable="evaluationFlag_Roll_7" function="true" activator="(!if_condition_8)"/>
        |  <Setter variable="evaluationFlag_Roll_7" function="true" activator="if_condition_8"/>
        |</Variables>""")
    }

    test("while loop") {
      funky {
        while Altitude <= 1000 do
          Pitch := 20
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_3" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_3_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_4" function="true" />
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_2) & while_condition_3)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="while_condition_3"/>
        |  <Setter variable="while_condition_3" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_3_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_5" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_3_4" function="false" activator="while_condition_3"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="false" activator="while_condition_3"/>
        |  <Setter variable="evaluationFlag_while_condition_3_5" function="false" activator="while_condition_3"/>
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
        |  <Setter variable="while_condition_5" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_5_6)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_6" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_3) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="while_condition_5"/>
        |  <Setter variable="while_condition_4" function="(Pitch &lt;= 12345.0)" activator="((!evaluationFlag_while_condition_4_7) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_7" function="true" activator="while_condition_5"/>
        |  <Setter variable="while_condition_4" function="(Pitch &lt;= 12345.0)" activator="((!evaluationFlag_while_condition_4_8) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="true" activator="while_condition_5"/>
        |  <Setter variable="evaluationFlag_while_condition_4_7" function="false" activator="(while_condition_4 & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="false" activator="(while_condition_4 & while_condition_5)"/>
        |  <Setter variable="while_condition_5" function="(Altitude &lt;= 1000.0)" activator="((!evaluationFlag_while_condition_5_9) & (!while_condition_4))"/>
        |  <Setter variable="evaluationFlag_while_condition_5_9" function="true" activator="(!while_condition_4)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_6" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_x_3" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_7" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_7" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_8" function="false" activator="((!while_condition_4) & while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_9" function="false" activator="((!while_condition_4) & while_condition_5)"/>
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
        |  <Setter variable="while_condition_9" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_9_10)"/>
        |  <Setter variable="evaluationFlag_while_condition_9_10" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_5) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" activator="while_condition_9"/>
        |  <Setter variable="while_condition_6" function="(Pitch &lt;= 12345.0)" activator="((!evaluationFlag_while_condition_6_11) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_11" function="true" activator="while_condition_9"/>
        |  <Setter variable="while_condition_6" function="(Pitch &lt;= 12345.0)" activator="((!evaluationFlag_while_condition_6_12) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_12" function="true" activator="while_condition_9"/>
        |  <Setter variable="evaluationFlag_while_condition_6_11" function="false" activator="(while_condition_6 & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_12" function="false" activator="(while_condition_6 & while_condition_9)"/>
        |  <Setter variable="x" function="50.0" activator="(((!evaluationFlag_x_8) & (!while_condition_6)) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_8" function="true" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="while_condition_9" function="(Altitude &lt;= 1000.0)" activator="((!evaluationFlag_while_condition_9_13) & (!while_condition_6))"/>
        |  <Setter variable="evaluationFlag_while_condition_9_13" function="true" activator="(!while_condition_6)"/>
        |  <Setter variable="evaluationFlag_while_condition_9_10" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_5" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_11" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_12" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_11" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_12" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_8" function="false" activator="((!while_condition_6) & while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_9_13" function="false" activator="((!while_condition_6) & while_condition_9)"/>
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
        |<Variables>
        |  <Setter variable="x" function="10.0" activator="(!evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_7" function="true" />
        |  <Setter variable="if_condition_11" function="(x &gt; 5.0)" activator="(!evaluationFlag_if_condition_11_15)"/>
        |  <Setter variable="evaluationFlag_if_condition_11_15" function="true" />
        |  <Setter variable="while_condition_10" function="(x &gt; 0.0)" activator="((!evaluationFlag_while_condition_10_13) & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_13" function="true" activator="if_condition_11"/>
        |  <Setter variable="x" function="(x - 1.0)" activator="(((!evaluationFlag_x_9) & while_condition_10) & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="(while_condition_10 & if_condition_11)"/>
        |  <Setter variable="while_condition_10" function="(x &gt; 0.0)" activator="((!evaluationFlag_while_condition_10_14) & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_14" function="true" activator="if_condition_11"/>
        |  <Setter variable="evaluationFlag_while_condition_10_13" function="false" activator="(while_condition_10 & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_x_9" function="false" activator="(while_condition_10 & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_14" function="false" activator="(while_condition_10 & if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_13" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_14" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_13" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_14" function="true" activator="(!if_condition_11)"/>
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_12) & (!while_condition_10))"/>
        |  <Setter variable="evaluationFlag_x_12" function="true" activator="(!while_condition_10)"/>
        |</Variables>""")
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
