package funkycompiler

import utest._
import testPrograms.*


object Stage12Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage(ifStatement).toString ==> "If(BinaryOp(Variable(b),Const(10.0),>),Assignment(Variable(a),Const(20.0)),Assignment(Variable(a),Const(10.0)))"
    }

    test("nested if statement") {
      stage(nestedIfStatement).toString ==> "If(BinaryOp(Variable(PitchRate),Const(0.01),>),If(BinaryOp(Variable(Flaps),Const(30.0),<),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),+)),Assignment(Variable(Autotrim),BinaryOp(Variable(Autotrim),Const(0.02),+))),If(BinaryOp(Variable(PitchRate),Const(-0.01),<),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),-)),Assignment(Variable(Flaps),Variable(Flaps))))"
    }

    test("blocks should accumulate assignments and if statements") {
      stage(blocksAccumulation).toString ==> "Block(List(Assignment(Variable(x),Const(10.0)), Assignment(Variable(y),Const(20.0)), Assignment(Variable(x),Const(30.0)), If(BinaryOp(Variable(x),Const(30.0),==),Block(List(Assignment(Variable(x),Const(40.0)))),null)))"
    }

    test("calls") {
      stage(calls).toString ==> "Assignment(Variable(x),Call(abs,List(Const(25.0))))"
    }
  }
end Stage12Suite
