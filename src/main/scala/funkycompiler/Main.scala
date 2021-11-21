package funkycompiler

import stage3.{ Tree, Variable, mkVarDefs }
import stage4.mkXml

// inline def ifStatement: Tree =
//   val a = Variable("a")
//   val b = Variable("b")
//   if b > 10 then a := 20 else a := 10

@main def main =
  println(mkXml(mkVarDefs(stage {
    val x = Variable("x")
    if x < 500 then
      x := x + 1
    else
      x := 0
  })))
