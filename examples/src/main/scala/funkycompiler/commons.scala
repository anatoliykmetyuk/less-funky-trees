package funkycompiler
package examples

import stdlib.*
import stage3.*

val testPlane = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/Test Plane.xml")

val elevators = Variable("elevators")
val ailerons = Variable("ailerons")
val rudder = Variable("rudder")
val thrust = Variable("thrust")

val rateOfDescent = rate(AltitudeAgl)

/*
DebugExpression sum(Pitch/250) – (1) proportional, how much
DebugExpression sum(Yaw/250) – (2) derivative, how careful
DebugExpression sum(Roll/250) – (3) integral, how quickly
*/
def PIDTune(target: Double, current: Tree, initP: Double = 0, initI: Double = 0, initD: Double = 0) =
  PID(target, current, initP+sum(Pitch/250), initI+sum(Roll/250), initD+sum(Yaw/250))
