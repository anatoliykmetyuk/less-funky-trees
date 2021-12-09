package funkycompiler
package examples
package planes

import stage3.*
import stdlib.*

object Pidgin:
  val file = plane("eeePidgin")

  def setLevelPitch: Tree = funky {
    while true do
      elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),
        0.1,0,0.05), 1)
  }

  def maintainBank(bnk: Tree) = funky {
    while true do ailerons := deltaangle(RollAngle, bnk) / 40
  }

  def maintainHeading(hdg: Tree) = funky {
    val bnk = Variable("bnk")
    val delta = deltaangle(hdg, Heading)
    maintainBank(bnk) & funky {
      while true do
        bnk := clamp(delta / 20, -1, 1) * 80
        elevators := PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),
          0.1,0,0.05)
        // if abs(abs(RollAngle) - 80) < 2 && abs(delta) > 30 then elevators := 1
    }
  }

  def goToCoordinates(lat: Tree, lon: Tree) = funky {
    val dLat = lat - Latitude
    val dLon = lon - Longitude
    val distance = sqrt(dLat*dLat + dLon*dLon)
    val hdg = Variable("hdg")

    maintainHeading(hdg) | funky {
      while distance > 500 do
        hdg := (if dLat >= 0 then asin(dLon / distance) else 180 - asin(dLon / distance))
    }
  }
