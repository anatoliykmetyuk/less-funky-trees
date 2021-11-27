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
        |  <Setter variable="a" function="20.0" activator="((!evaluationFlag_a_1) &amp; if_condition_3)"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="if_condition_3"/>
        |  <Setter variable="evaluationFlag_a_1" function="true" activator="(!if_condition_3)"/>
        |  <Setter variable="a" function="10.0" activator="((!evaluationFlag_a_2) &amp; (!if_condition_3))"/>
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
        |  <Setter variable="if_condition_3" function="(Flaps &lt; 30.0)" activator="((!evaluationFlag_if_condition_3_8) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="if_condition_7"/>
        |  <Setter variable="Flaps" function="(Flaps + 0.02)" activator="(((!evaluationFlag_Flaps_1) &amp; if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(if_condition_3 &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="((!if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="Autotrim" function="(Autotrim + 0.02)" activator="(((!evaluationFlag_Autotrim_2) &amp; (!if_condition_3)) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="((!if_condition_3) &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(if_condition_3 &amp; if_condition_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_3_8" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Flaps_1" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="evaluationFlag_Autotrim_2" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="if_condition_6" function="(PitchRate &lt; -0.01)" activator="((!evaluationFlag_if_condition_6_9) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_if_condition_6_9" function="true" activator="(!if_condition_7)"/>
        |  <Setter variable="Flaps" function="(Flaps - 0.02)" activator="(((!evaluationFlag_Flaps_4) &amp; if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="(if_condition_6 &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_4" function="true" activator="((!if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="Flaps" function="Flaps" activator="(((!evaluationFlag_Flaps_5) &amp; (!if_condition_6)) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="((!if_condition_6) &amp; (!if_condition_7))"/>
        |  <Setter variable="evaluationFlag_Flaps_5" function="true" activator="(if_condition_6 &amp; (!if_condition_7))"/>
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
        |  <Setter variable="y" function="20.0" activator="((!evaluationFlag_y_8) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_y_8" function="true" activator="evaluationFlag_x_7"/>
        |  <Setter variable="x" function="30.0" activator="((!evaluationFlag_x_9) &amp; evaluationFlag_y_8)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="evaluationFlag_y_8"/>
        |  <Setter variable="if_condition_12" function="(x = 30.0)" activator="((!evaluationFlag_if_condition_12_13) &amp; evaluationFlag_x_9)"/>
        |  <Setter variable="evaluationFlag_if_condition_12_13" function="true" activator="evaluationFlag_x_9"/>
        |  <Setter variable="x" function="40.0" activator="(((!evaluationFlag_x_11) &amp; if_condition_12) &amp; evaluationFlag_x_9)"/>
        |  <Setter variable="evaluationFlag_x_11" function="true" activator="(if_condition_12 &amp; evaluationFlag_x_9)"/>
        |  <Setter variable="evaluationFlag_x_11" function="true" activator="((!if_condition_12) &amp; evaluationFlag_x_9)"/>
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
        |  <Setter variable="x" function="2.0" activator="((!evaluationFlag_x_2) &amp; evaluationFlag_x_1)"/>
        |  <Setter variable="evaluationFlag_x_2" function="true" activator="evaluationFlag_x_1"/>
        |  <Setter variable="x" function="3.0" activator="((!evaluationFlag_x_3) &amp; evaluationFlag_x_2)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="evaluationFlag_x_2"/>
        |  <Setter variable="x" function="4.0" activator="((!evaluationFlag_x_4) &amp; evaluationFlag_x_3)"/>
        |  <Setter variable="evaluationFlag_x_4" function="true" activator="evaluationFlag_x_3"/>
        |  <Setter variable="x" function="5.0" activator="((!evaluationFlag_x_5) &amp; evaluationFlag_x_4)"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" activator="evaluationFlag_x_4"/>
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
        |  <Setter variable="Pitch" function="10.0" activator="((!evaluationFlag_Pitch_2) &amp; if_condition_5)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="if_condition_5"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="(!if_condition_5)"/>
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_4) &amp; (!if_condition_5))"/>
        |  <Setter variable="evaluationFlag_Pitch_4" function="true" activator="(!if_condition_5)"/>
        |  <Setter variable="evaluationFlag_Pitch_4" function="true" activator="if_condition_5"/>
        |  <Setter variable="if_condition_8" function="((10.0 = 0.0)?false:true)" activator="((!evaluationFlag_if_condition_8_9) &amp; (evaluationFlag_Pitch_2 | evaluationFlag_Pitch_4))"/>
        |  <Setter variable="evaluationFlag_if_condition_8_9" function="true" activator="(evaluationFlag_Pitch_2 | evaluationFlag_Pitch_4)"/>
        |  <Setter variable="Roll" function="1.0" activator="((!evaluationFlag_Roll_6) &amp; if_condition_8)"/>
        |  <Setter variable="evaluationFlag_Roll_6" function="true" activator="if_condition_8"/>
        |  <Setter variable="evaluationFlag_Roll_6" function="true" activator="(!if_condition_8)"/>
        |  <Setter variable="Roll" function="20.0" activator="((!evaluationFlag_Roll_7) &amp; (!if_condition_8))"/>
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
        |  <Setter variable="while_condition_3" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_3_6)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_6" function="true" />
        |  <Setter variable="Pitch" function="20.0" activator="((!evaluationFlag_Pitch_2) &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="true" activator="while_condition_3"/>
        |  <Setter variable="while_condition_3" function="(Altitude &lt;= 1000.0)" activator="((!evaluationFlag_while_condition_3_7) &amp; evaluationFlag_Pitch_2)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="true" activator="evaluationFlag_Pitch_2"/>
        |  <Setter variable="memoised_whileBodyEvaluated_4" function="evaluationFlag_Pitch_2" activator="(!evaluationFlag_memoised_whileBodyEvaluated_4_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_4_5" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_3_6" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_Pitch_2" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_while_condition_3_7" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_4_5" function="false" activator="(memoised_whileBodyEvaluated_4 &amp; while_condition_3)"/>
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
        |  <Setter variable="while_condition_5" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_5_8)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_8" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_x_3" function="true" activator="while_condition_5"/>
        |  <Setter variable="while_condition_4" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_4_11) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_11" function="true" activator="(evaluationFlag_x_3 &amp; while_condition_5)"/>
        |  <Setter variable="while_condition_4" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_4_12) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_12" function="true" activator="(evaluationFlag_x_3 &amp; while_condition_5)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_9" function="true" activator="(((!evaluationFlag_memoised_whileBodyEvaluated_9_10) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_9_10" function="true" activator="(evaluationFlag_x_3 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_11" function="false" activator="(((memoised_whileBodyEvaluated_9 &amp; while_condition_4) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_12" function="false" activator="(((memoised_whileBodyEvaluated_9 &amp; while_condition_4) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_9_10" function="false" activator="(((memoised_whileBodyEvaluated_9 &amp; while_condition_4) &amp; evaluationFlag_x_3) &amp; while_condition_5)"/>
        |  <Setter variable="while_condition_5" function="(Altitude &lt;= 1000.0)" activator="((!evaluationFlag_while_condition_5_13) &amp; (evaluationFlag_x_3 &amp; (!while_condition_4)))"/>
        |  <Setter variable="evaluationFlag_while_condition_5_13" function="true" activator="(evaluationFlag_x_3 &amp; (!while_condition_4))"/>
        |  <Setter variable="memoised_whileBodyEvaluated_6" function="(evaluationFlag_x_3 &amp; (!while_condition_4))" activator="(!evaluationFlag_memoised_whileBodyEvaluated_6_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_6_7" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_5_8" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_x_3" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_11" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_12" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_9_10" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_11" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_4_12" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_9_10" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_while_condition_5_13" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_6_7" function="false" activator="(memoised_whileBodyEvaluated_6 &amp; while_condition_5)"/>
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
        |  <Setter variable="while_condition_9" function="(Altitude &lt;= 1000.0)" activator="(!evaluationFlag_while_condition_9_12)"/>
        |  <Setter variable="evaluationFlag_while_condition_9_12" function="true" />
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_5" function="true" activator="while_condition_9"/>
        |  <Setter variable="while_condition_6" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_6_15) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_15" function="true" activator="(evaluationFlag_x_5 &amp; while_condition_9)"/>
        |  <Setter variable="while_condition_6" function="(Pitch &lt;= 12345.0)" activator="(((!evaluationFlag_while_condition_6_16) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_16" function="true" activator="(evaluationFlag_x_5 &amp; while_condition_9)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_13" function="true" activator="(((!evaluationFlag_memoised_whileBodyEvaluated_13_14) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="true" activator="(evaluationFlag_x_5 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_15" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_6) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_16" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_6) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_6) &amp; evaluationFlag_x_5) &amp; while_condition_9)"/>
        |  <Setter variable="x" function="50.0" activator="(((!evaluationFlag_x_8) &amp; (!while_condition_6)) &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_8" function="true" activator="((!while_condition_6) &amp; while_condition_9)"/>
        |  <Setter variable="while_condition_9" function="(Altitude &lt;= 1000.0)" activator="((!evaluationFlag_while_condition_9_17) &amp; ((evaluationFlag_x_5 &amp; (!while_condition_6)) &amp; evaluationFlag_x_8))"/>
        |  <Setter variable="evaluationFlag_while_condition_9_17" function="true" activator="((evaluationFlag_x_5 &amp; (!while_condition_6)) &amp; evaluationFlag_x_8)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_10" function="((evaluationFlag_x_5 &amp; (!while_condition_6)) &amp; evaluationFlag_x_8)" activator="(!evaluationFlag_memoised_whileBodyEvaluated_10_11)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_10_11" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_9_12" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_5" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_15" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_16" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_15" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_6_16" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_x_8" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_9_17" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_10_11" function="false" activator="(memoised_whileBodyEvaluated_10 &amp; while_condition_9)"/>
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
        |  <Setter variable="if_condition_11" function="(x &gt; 5.0)" activator="((!evaluationFlag_if_condition_11_17) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_if_condition_11_17" function="true" activator="evaluationFlag_x_7"/>
        |  <Setter variable="while_condition_10" function="(x &gt; 0.0)" activator="(((!evaluationFlag_while_condition_10_15) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_15" function="true" activator="(if_condition_11 &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="x" function="(x - 1.0)" activator="((((!evaluationFlag_x_9) &amp; while_condition_10) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="((while_condition_10 &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="while_condition_10" function="(x &gt; 0.0)" activator="((((!evaluationFlag_while_condition_10_16) &amp; evaluationFlag_x_9) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_16" function="true" activator="((evaluationFlag_x_9 &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_13" function="evaluationFlag_x_9" activator="(((!evaluationFlag_memoised_whileBodyEvaluated_13_14) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="true" activator="(if_condition_11 &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_15" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_10) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_9" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_10) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_16" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_10) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="false" activator="(((memoised_whileBodyEvaluated_13 &amp; while_condition_10) &amp; if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_15" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_16" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_15" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_x_9" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_16" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="true" activator="((!if_condition_11) &amp; evaluationFlag_x_7)"/>
        |  <Setter variable="x" function="0.0" activator="((!evaluationFlag_x_12) &amp; ((!while_condition_10) &amp; evaluationFlag_x_9))"/>
        |  <Setter variable="evaluationFlag_x_12" function="true" activator="((!while_condition_10) &amp; evaluationFlag_x_9)"/>
        |</Variables>""")
    }

    test("parallel and") {
      val elevators = Variable("elevators")
      val ailerons = Variable("ailerons")
      val thrust = Variable("thrust")
      val levelFlight = funky {
        while true do
          elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
          thrust := 1
      }
      val wingLevelling = funky {
        while true do
          ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
      }

      funky {
        levelFlight & wingLevelling
        thrust := 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_7" function="true" activator="(!evaluationFlag_while_condition_7_15)"/>
        |  <Setter variable="evaluationFlag_while_condition_7_15" function="true" />
        |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="((!evaluationFlag_elevators_4) &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_elevators_4" function="true" activator="while_condition_7"/>
        |  <Setter variable="thrust" function="1.0" activator="(((!evaluationFlag_thrust_6) &amp; evaluationFlag_elevators_4) &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_thrust_6" function="true" activator="(evaluationFlag_elevators_4 &amp; while_condition_7)"/>
        |  <Setter variable="while_condition_7" function="true" activator="((!evaluationFlag_while_condition_7_16) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))"/>
        |  <Setter variable="evaluationFlag_while_condition_7_16" function="true" activator="(evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)"/>
        |  <Setter variable="memoised_whileBodyEvaluated_13" function="(evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)" activator="(!evaluationFlag_memoised_whileBodyEvaluated_13_14)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_7_15" function="false" activator="(memoised_whileBodyEvaluated_13 &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_elevators_4" function="false" activator="(memoised_whileBodyEvaluated_13 &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_thrust_6" function="false" activator="(memoised_whileBodyEvaluated_13 &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_while_condition_7_16" function="false" activator="(memoised_whileBodyEvaluated_13 &amp; while_condition_7)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_13_14" function="false" activator="(memoised_whileBodyEvaluated_13 &amp; while_condition_7)"/>
        |  <Setter variable="while_condition_10" function="true" activator="(!evaluationFlag_while_condition_10_19)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_19" function="true" />
        |  <Setter variable="ailerons" function="(smooth(PID(0.0,RollRate,0.001,0.0,0.0),0.1) + Roll)" activator="((!evaluationFlag_ailerons_9) &amp; while_condition_10)"/>
        |  <Setter variable="evaluationFlag_ailerons_9" function="true" activator="while_condition_10"/>
        |  <Setter variable="while_condition_10" function="true" activator="((!evaluationFlag_while_condition_10_20) &amp; evaluationFlag_ailerons_9)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_20" function="true" activator="evaluationFlag_ailerons_9"/>
        |  <Setter variable="memoised_whileBodyEvaluated_17" function="evaluationFlag_ailerons_9" activator="(!evaluationFlag_memoised_whileBodyEvaluated_17_18)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_17_18" function="true" />
        |  <Setter variable="evaluationFlag_while_condition_10_19" function="false" activator="(memoised_whileBodyEvaluated_17 &amp; while_condition_10)"/>
        |  <Setter variable="evaluationFlag_ailerons_9" function="false" activator="(memoised_whileBodyEvaluated_17 &amp; while_condition_10)"/>
        |  <Setter variable="evaluationFlag_while_condition_10_20" function="false" activator="(memoised_whileBodyEvaluated_17 &amp; while_condition_10)"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_17_18" function="false" activator="(memoised_whileBodyEvaluated_17 &amp; while_condition_10)"/>
        |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_12) &amp; (((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) &amp; ((!while_condition_10) &amp; evaluationFlag_ailerons_9)))"/>
        |  <Setter variable="evaluationFlag_thrust_12" function="true" activator="(((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) &amp; ((!while_condition_10) &amp; evaluationFlag_ailerons_9))"/>
        |</Variables>""")
    }

    test("parallel or") {
      val elevators = Variable("elevators")
      val thrust = Variable("thrust")
      val levelFlight = funky {
        while true do
          elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
          thrust := 1
      }
      val maintainFor5Seconds = funky {
        val t = Variable("start")
        t := Time
        while Time - t < 5 do ()
      }

      funky {
        levelFlight | maintainFor5Seconds
        thrust := 0
      }.compile ==> testStr("""
        |<Variables>
        |  <Setter variable="while_condition_7" function="true" activator="((!evaluationFlag_while_condition_7_16) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_while_condition_7_16" function="true" activator="(!(evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
        |  <Setter variable="elevators" function="(smooth(PID(0.0,(PitchAngle + smooth(AngleOfAttack,0.1)),0.1,0.0,0.1),0.1) + Pitch)" activator="(((!evaluationFlag_elevators_4) &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_elevators_4" function="true" activator="(while_condition_7 &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="thrust" function="1.0" activator="((((!evaluationFlag_thrust_6) &amp; evaluationFlag_elevators_4) &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_thrust_6" function="true" activator="((evaluationFlag_elevators_4 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="while_condition_7" function="true" activator="(((!evaluationFlag_while_condition_7_17) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_while_condition_7_17" function="true" activator="((evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="memoised_whileBodyEvaluated_14" function="(evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)" activator="((!evaluationFlag_memoised_whileBodyEvaluated_14_15) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_14_15" function="true" activator="(!(evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
        |  <Setter variable="evaluationFlag_while_condition_7_16" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_elevators_4" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_thrust_6" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_while_condition_7_17" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_14_15" function="false" activator="((memoised_whileBodyEvaluated_14 &amp; while_condition_7) &amp; (!(evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="start" function="Time" activator="((!evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_start_10" function="true" activator="(!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)))"/>
        |  <Setter variable="while_condition_11" function="((Time - start) &lt; 5.0)" activator="(((!evaluationFlag_while_condition_11_20) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_while_condition_11_20" function="true" activator="(evaluationFlag_start_10 &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="while_condition_11" function="((Time - start) &lt; 5.0)" activator="(((!evaluationFlag_while_condition_11_21) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_while_condition_11_21" function="true" activator="(evaluationFlag_start_10 &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="memoised_whileBodyEvaluated_18" function="true" activator="(((!evaluationFlag_memoised_whileBodyEvaluated_18_19) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_18_19" function="true" activator="(evaluationFlag_start_10 &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_while_condition_11_20" function="false" activator="(((memoised_whileBodyEvaluated_18 &amp; while_condition_11) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_while_condition_11_21" function="false" activator="(((memoised_whileBodyEvaluated_18 &amp; while_condition_11) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="evaluationFlag_memoised_whileBodyEvaluated_18_19" function="false" activator="(((memoised_whileBodyEvaluated_18 &amp; while_condition_11) &amp; evaluationFlag_start_10) &amp; (!((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6))))"/>
        |  <Setter variable="thrust" function="0.0" activator="((!evaluationFlag_thrust_13) &amp; (((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) | (evaluationFlag_start_10 &amp; (!while_condition_11))))"/>
        |  <Setter variable="evaluationFlag_thrust_13" function="true" activator="(((!while_condition_7) &amp; (evaluationFlag_elevators_4 &amp; evaluationFlag_thrust_6)) | (evaluationFlag_start_10 &amp; (!while_condition_11)))"/>
        |</Variables>""")
    }
  }
