package funkycompiler
package examples

import stage3.*
import stdlib.*


@main def ManualControl = program(testPlane) {
  while true do
    elevators := -Pitch
    ailerons := Roll
    rudder := Yaw
    thrust := Throttle
}
