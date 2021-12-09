package funkycompiler
package examples
package planes

import stage3.*
import stdlib.*

object Pidgin:
  val file = plane("eeePidgin")
  val slowTurnAngle = 5
  val fastTurnAngle = 80

  def setLevelPitch: Tree = funky {
    while true do
      thrust := 1
      elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),
        0.1,0.01,0.05), 0.25) - Pitch
  }

  def bank(tgt: Tree) = funky {
    val delta = freshVar()
    val direction = freshVar()
    val marker = Variable("bank_marker")

    marker := 0
    while
      delta := deltaangle(RollAngle, tgt)
      direction := sign(delta)
      abs(delta) > 1 || abs(RollRate) > 10
    do
      marker :+= 1
      ailerons := (
        if abs(delta) > 40 then 1 * direction
        else if abs(delta) > 1 then 0.1 * direction
        else 0)
      marker :+= 1
  }

  def setHeading(hdg: Tree) = funky {
    val delta = freshVar()
    val direction = freshVar()
    val marker = Variable("marker")
    val marker2 = Variable("marker2")
    val bank_marker = Variable("bank_marker")

    marker := 0
    while
      delta := deltaangle(hdg, Heading)
      direction := sign(delta)
      abs(delta) > 1 || abs(rate(Heading)) > 10
    do
      marker :+= 1
      if abs(delta) > 5 then bank(direction * 80)
      else if abs(delta) > 1 then bank(direction * 40)
      marker :+= 1
    marker2 := 1
    bank(0)
    marker2 :+= 1
  }

  def goToCoordinates(lat: Tree, lon: Tree) = funky {
    val distance = Variable("distance")
    val dLat = Variable("dLat")
    val dLon = Variable("dLon")
    val hdg = Variable("coordHeading")
    while
      dLat := lat - Latitude
      dLon := lon - Longitude
      distance := sqrt(dLat*dLat + dLon*dLon)
      hdg := (if dLat >= 0 then asin(dLon / distance) else 180 - asin(dLon / distance))
      distance > 500
    do
      setHeading(hdg)
  }
