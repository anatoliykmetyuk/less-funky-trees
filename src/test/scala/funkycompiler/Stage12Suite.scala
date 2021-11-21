package funkycompiler

import utest._
import stage3.*
import stdlib.*

object Stage12Suite extends TestSuite:
  val tests = Tests {
    test("if statement"){
      stage {
        val a = Variable("a")
        val b = Variable("b")
        if b > 10 then a := 20 else a := 10
      }.toString ==> "If(BinaryOp(Variable(b),Number(10.0),>),Assignment(Variable(a),Number(20.0)),Assignment(Variable(a),Number(10.0)))"
    }

    test("nested if statement") {
      stage {
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
      }.toString ==> "If(BinaryOp(Variable(PitchRate),Number(0.01),>),If(BinaryOp(Variable(Flaps),Number(30.0),<),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Number(0.02),+)),Assignment(Variable(Autotrim),BinaryOp(Variable(Autotrim),Number(0.02),+))),If(BinaryOp(Variable(PitchRate),Number(-0.01),<),Assignment(Variable(Flaps),BinaryOp(Variable(Flaps),Number(0.02),-)),Assignment(Variable(Flaps),Variable(Flaps))))"
    }

    test("blocks should accumulate assignments and if statements") {
      stage {
        val x = Variable("x")
        val y = Variable("y")
        x := 10
        y := 20
        x := 30
        if x === 30 then
          x := 40
        50
      }.toString ==> "Block(List(Assignment(Variable(x),Number(10.0)), Assignment(Variable(y),Number(20.0)), Assignment(Variable(x),Number(30.0)), If(BinaryOp(Variable(x),Number(30.0),==),Block(List(Assignment(Variable(x),Number(40.0)))),null)))"
    }

    test("calls") {
      stage {
        val x = Variable("x")
        x := abs(25)
      }.toString ==> "Assignment(Variable(x),Call(Function(abs),List(Number(25.0))))"
    }
  }
end Stage12Suite
