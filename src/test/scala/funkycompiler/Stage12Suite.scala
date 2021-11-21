package funkycompiler

import utest._
import testPrograms.*


object Stage12Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage(ifStatement).toString ==> "If(BinaryOp(Variable(b),Const(10.0),&gt;),Assignment(Variable(a),Const(20.0)),Assignment(Variable(a),Const(10.0)))"
    }

    test("nested if statement") {
      stage(nestedIfStatement).toString ==> "If(BinaryOp(Variable(PitchRate),Const(0.01),&gt;),If(BinaryOp(Variable(Flaps),Const(30.0),&lt;),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),+)),Assignment(Variable(Autotrim),BinaryOp(Variable(Autotrim),Const(0.02),+))),If(BinaryOp(Variable(PitchRate),Const(-0.01),&lt;),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),-)),Assignment(Variable(Flaps),Variable(Flaps))))"
    }

    test("blocks should accumulate assignments and if statements") {
      stage(blocksAccumulation).toString ==> "Block(List(Assignment(Variable(x),Const(10.0)), Assignment(Variable(y),Const(20.0)), Assignment(Variable(x),Const(30.0)), If(BinaryOp(Variable(x),Const(30.0),=),Block(List(Assignment(Variable(x),Const(40.0)))),null)))"
    }

    test("calls") {
      stage(calls).toString ==> "Assignment(Variable(x),Call(abs,List(Const(25.0))))"
    }

    test("loop unrolling") {
      stage(loopUnrolling).toString ==> "Block(List(Assignment(Variable(x),Const(1.0)), Assignment(Variable(x),Const(2.0)), Assignment(Variable(x),Const(3.0)), Assignment(Variable(x),Const(4.0)), Assignment(Variable(x),Const(5.0))))"
    }
  }
end Stage12Suite
