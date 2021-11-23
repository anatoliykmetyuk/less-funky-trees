package funkycompiler
package examples

import stage3.{ Tree, Variable, mkVarDefs }
import stage4.mkXml
import stdlib.*


@main def LevelFlight =
  val testPlane = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/Test Plane.xml")
  program(testPlane) {
    elevators := smooth(PIDTune(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.05,0,0.1), 0.01)
    thrust := 1
  }
