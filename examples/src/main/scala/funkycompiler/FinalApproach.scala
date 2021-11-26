package funkycompiler
package examples

import stage3.{ Tree, Variable }
import stdlib.*


@main def FinalApproach = program(testPlane) {
  val elevatorsStep = 0.001
  val targetRateOfDescent = -6
  val maxAllowedPitch = 0
  val maxAllowedRateOfDescent = -25
  val cutEnginesAltitude = 3

  elevators := 0

  while true do
    ailerons := Roll
    rudder := Yaw


    if PitchAngle < maxAllowedPitch then
      elevators := elevators + elevatorsStep
    else if PitchAngle > 5 & elevators > 0 then
      elevators := elevators - elevatorsStep
    elevators := clamp01(elevators)

    if AltitudeAgl < cutEnginesAltitude then thrust := 0
    else thrust := (
      if rate(AltitudeAgl) > targetRateOfDescent then 0.08
      else if rate(AltitudeAgl) < maxAllowedRateOfDescent then 0.25
      else 0.12)
}
