package funkycompiler

import stage3.Variable

@main def main =
  println(stage {
    val x = Variable("x")
    if x === 30 then
      x := 40
    50
  })
