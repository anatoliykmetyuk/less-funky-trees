package funkycompiler

import stage3.{ Tree, Variable, mkVarDefs }
import stage4.mkXml


@main def main =
  val testPlane = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/Test Plane.xml")
  val program = mkXml(mkVarDefs(stage {
    val x = Variable("x")
    if x < 500 then
      x := x + 1
    else
      x := 0
  }))
  writeVariables(testPlane, program)
