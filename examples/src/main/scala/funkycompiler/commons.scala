package funkycompiler
package examples

import stage3.*
import stdlib.*

val testPlane = java.io.File("/Users/kmetiuk/Library/Application Support/unity.Jundroo.SimplePlanes/AircraftDesigns/Test Plane.xml")

val elevators = Variable("elevators")
val ailerons = Variable("ailerons")
val rudder = Variable("rudder")
val thrust = Variable("thrust")

val rateOfDescent = rate(AltitudeAgl)


def updateBank(tgt: Tree, maxDeviation: Tree = 5, deflection: Tree = 0.3): Tree = funky {
  if abs(deltaangle(RollAngle, tgt)) > maxDeviation then
    ailerons := sign(RollAngle - tgt) * deflection
  else
    ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
}

def setHeading(tgt: Tree, headingSetFlag: Variable | Null = null, maxDeviation: Tree = 0.5): Tree = funky {
  val delta = freshVar()
  val correctionAngle = freshVar()
  val aileronDeflection = freshVar()
  val maxBankDeviation = freshVar()

  while true do
    delta := deltaangle(tgt, Heading)
    if abs(delta) > maxDeviation then
      if abs(delta) < 10 then
        correctionAngle := lerp(5, 10, 0.5)
        aileronDeflection := 0.1
        maxBankDeviation := 1
      else
        correctionAngle := 60
        aileronDeflection := 0.3
        maxBankDeviation := 5
      updateBank(sign(delta) * correctionAngle, maxBankDeviation, aileronDeflection)
      if headingSetFlag != null then
        headingSetFlag := false
    else
      updateBank(0)
      if headingSetFlag != null then
        headingSetFlag := abs(deltaangle(RollAngle, 0)) < maxBankDeviation
}

def levelPitch: Tree = funky {
  while true do
    thrust := 1
    elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) - Pitch
}

def waitFor(time: Tree): Tree = funky {
  val start = freshVar()
  start := Time
  while Time - start < time do ()
}
