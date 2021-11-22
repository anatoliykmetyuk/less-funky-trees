package funkycompiler

import stage3.{ Tree, Variable, mkVarDefs }
import stage4.mkXml
import stdlib.*


@main def main =
  val testPlane = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/Test Plane.xml")
  val program = mkXml(mkVarDefs(stage {
    val elevators = Variable("elevators")
    val ailerons = Variable("ailerons")
    val rudder = Variable("rudder")
    val thrust = Variable("thrust")

    val elevatorsStep = 0.001
    val targetRateOfDescent = -6
    val maxAllowedPitch = 0
    val maxAllowedRateOfDescent = -25
    val cutEnginesAltitude = 3

    ailerons := Roll
    rudder := Yaw

    if Time === 0 then
      elevators := 0


    if PitchAngle < maxAllowedPitch then
      elevators := elevators + elevatorsStep
    else if PitchAngle > 5 & elevators > 0 then
      elevators := elevators - elevatorsStep
    elevators := clamp01(elevators + Pitch)

    if AltitudeAgl < cutEnginesAltitude then thrust := 0
    else thrust := (
      if rate(AltitudeAgl) > targetRateOfDescent then 0.08
      else if rate(AltitudeAgl) < maxAllowedRateOfDescent then 0.25
      else 0.12)
  }))
  writeVariables(testPlane, program)
