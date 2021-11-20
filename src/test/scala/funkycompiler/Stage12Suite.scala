package funkycompiler

import utest._
import stage3.*

object Stage12Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage {
        val a = Variable("a")
        val b = Variable("b")
        if b > 10 then a := 20 else a := 10
      }.toString ==> "IfStat(BinaryOp(Variable(b),LNumber(10.0),>),Stats(List(Assignment(Variable(a),LNumber(20.0)))),Stats(List(Assignment(Variable(a),LNumber(10.0)))))"
    }
  }
end Stage12Suite
