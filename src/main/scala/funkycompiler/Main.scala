package funkycompiler

import stage3.{ Variable, mkVarDefs }

@main def main =
  println(mkVarDefs(stage {
    val x = Variable("x")
    x := { if x === 30 then 10 else 20 }
    if x === 30 then
      x := 40
    else
      x := x
    50
  }))
