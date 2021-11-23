package funkycompiler

import stage3.{ Variable, Tree }
import stdlib.*


object testPrograms:
  transparent inline def ifStatement =
    val a = Variable("a")
    val b = Variable("b")
    if b > 10 then a := 20 else a := 10

  transparent inline def nestedIfStatement =
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

  transparent inline def blocksAccumulation =
    val x = Variable("x")
    val y = Variable("y")
    x := 10
    y := 20
    x := 30
    if x === 30 then
      x := 40
    50

  transparent inline def calls =
    val x = Variable("x")
    x := abs(25)

  transparent inline def loopUnrolling =
    val x = Variable("x")
    for i <- 1 to 5 yield
      x := i

  def f(x: Tree): Tree = stage {
    if x === 0 then
      Pitch := 10
      false
    else
      Pitch := 20
      true
  }

  transparent inline def booleanFunction =
    if f(10) then
      Roll := 1
    else
      Roll := 20

