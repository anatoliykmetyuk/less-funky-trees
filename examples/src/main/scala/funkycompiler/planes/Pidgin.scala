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

    while
      delta := deltaangle(RollAngle, tgt)
      direction := sign(delta)
      abs(delta) > 1 || abs(RollRate) > 10
    do
      ailerons := (
        if abs(delta) > 40 then 1 * direction
        else if abs(delta) > 1 then 0.1 * direction
        else 0)
  }

  // def turn(direction: Tree) = bank(fastTurnAngle * sign(direction))
  // def slowTurn(direction: Tree) = bank(slowTurnAngle * sign(direction))
  // def levelWings = bank(0)

  def setHeading(hdg: Tree) = funky {
    val delta = freshVar()
    val direction = freshVar()

    while
      delta := deltaangle(hdg, Heading)
      direction := sign(delta)
      abs(delta) > 1 || abs(rate(Heading)) > 10
    do
      if abs(delta) > 10 then bank(direction * 80)
      else if abs(delta) > 1 then bank(direction * 10)
    bank(0)
  }

  // def takeOff: Tree = funky {
  //   thrust := 1
  //   while Altitude < 300 do
  //     if TAS > 50 then updatePitch(30)
  // }


