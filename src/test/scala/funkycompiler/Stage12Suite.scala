package funkycompiler

import utest._
import testPrograms.*


object Stage12Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage(ifStatement).toString ==> "If(BinaryOp(Variable(b),Const(10.0),&gt;),Block(List(Assignment(Variable(a),Const(20.0)), Variable(a))),Block(List(Assignment(Variable(a),Const(10.0)), Variable(a))))"
    }

    test("nested if statement") {
      stage(nestedIfStatement).toString ==> "If(BinaryOp(Variable(PitchRate),Const(0.01),&gt;),If(BinaryOp(Variable(Flaps),Const(30.0),&lt;),Block(List(Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),+)), Variable(Flaps))),Block(List(Assignment(Variable(Autotrim),BinaryOp(Variable(Autotrim),Const(0.02),+)), Variable(Autotrim)))),If(BinaryOp(Variable(PitchRate),Const(-0.01),&lt;),Block(List(Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Const(0.02),-)), Variable(Flaps))),Block(List(Assignment(Variable(Flaps),Variable(Flaps)), Variable(Flaps)))))"
    }

    test("blocks should accumulate assignments and if statements") {
      stage(blocksAccumulation).toString ==> "Block(List(Block(List(Assignment(Variable(x),Const(10.0)), Variable(x))), Block(List(Assignment(Variable(y),Const(20.0)), Variable(y))), Block(List(Assignment(Variable(x),Const(30.0)), Variable(x))), If(BinaryOp(Variable(x),Const(30.0),=),Block(List(Block(List(Assignment(Variable(x),Const(40.0)), Variable(x))))),null), Const(50.0)))"
    }

    test("calls") {
      stage(calls).toString ==> "Block(List(Assignment(Variable(x),Call(abs,List(Const(25.0)))), Variable(x)))"
    }

    test("loop unrolling") {
      stage(loopUnrolling).toString ==> "Block(List(Block(List(Assignment(Variable(x),Const(1.0)), Variable(x))), Block(List(Assignment(Variable(x),Const(2.0)), Variable(x))), Block(List(Assignment(Variable(x),Const(3.0)), Variable(x))), Block(List(Assignment(Variable(x),Const(4.0)), Variable(x))), Block(List(Assignment(Variable(x),Const(5.0)), Variable(x)))))"
    }

    test("boolean function") {
      stage(booleanFunction).toString ==> "If(If(BinaryOp(Const(10.0),Const(0.0),=),Block(List(Block(List(Assignment(Variable(Pitch),Const(10.0)), Variable(Pitch))), Const(false))),Block(List(Block(List(Assignment(Variable(Pitch),Const(20.0)), Variable(Pitch))), Const(true)))),Block(List(Assignment(Variable(Roll),Const(1.0)), Variable(Roll))),Block(List(Assignment(Variable(Roll),Const(20.0)), Variable(Roll))))"
    }
  }
end Stage12Suite
