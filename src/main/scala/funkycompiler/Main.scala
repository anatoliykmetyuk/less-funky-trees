package funkycompiler

import stage3.Variable

@main def main =
  println(stage {
    val x = Variable("x")
    x := { if x === 30 then 10 else 20 }
    if x === 30 then
      x := 40
    else
      x := x
    50
  })
