package funkycompiler
package examples

import stage3.*
import stdlib.*
import planes.Pidgin.{ file => pidgin, * }


@main def ManualControl = program(pidgin) {
  while true do
    elevators := -Pitch
    ailerons := Roll
    rudder := Yaw
    thrust := Throttle
}
