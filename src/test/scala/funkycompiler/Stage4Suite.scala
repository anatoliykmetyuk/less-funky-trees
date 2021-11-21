package funkycompiler

import utest._
import testPrograms.*


object Stage4Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage4.mkXml(stage3.mkVarDefs(stage(ifStatement))).toString ==> ""
    }

    test("nested if statement") {
      stage4.mkXml(stage3.mkVarDefs(stage(nestedIfStatement))).toString ==> ""
    }

    test("blocks should accumulate assignments and if statements") {
      stage4.mkXml(stage3.mkVarDefs(stage(blocksAccumulation))).toString ==> ""
    }

    test("calls") {
      stage4.mkXml(stage3.mkVarDefs(stage(calls))).toString ==> ""
    }

    test("loop unrolling") {
      stage4.mkXml(stage3.mkVarDefs(stage(loopUnrolling))).toString ==> ""
    }
  }
