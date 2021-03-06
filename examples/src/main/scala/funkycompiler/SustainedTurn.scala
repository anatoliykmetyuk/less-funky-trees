package funkycompiler
package examples

import stage3.{ Tree, Variable }
import stdlib.*
import planes.TestPlane.{ file => testPlane, * }


@main def SustainedTurn = program(testPlane) {
  thrust := 1
  val targetAngle = -60
  val maxDeviation = 5

  while true do
    elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch

    if abs(deltaangle(RollAngle, targetAngle)) > maxDeviation then
      ailerons := sign(RollAngle - targetAngle) * 0.3
    else
      ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
}
