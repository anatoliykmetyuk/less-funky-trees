package funkycompiler
package examples

import stage3.{ Tree, Variable }
import stdlib.*


@main def LevelFlight = program(testPlane) {
  elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
  thrust := 1
}
