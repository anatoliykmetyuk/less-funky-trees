package funkycompiler
package examples

import stage3.{ Tree, Variable, mkVarDefs, Assignment }
import stage4.mkXml
import stdlib.*


def bankSet(tgt: Tree, maxDeviation: Tree = 5, deflection: Tree = 0.3): Tree = stage {
  if abs(deltaangle(RollAngle, tgt)) > maxDeviation then
    ailerons := sign(RollAngle - tgt) * deflection
    false
  else
    ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
    true
}

def headingSet(tgt: Tree, maxDeviation: Tree = 0.5): Tree = stage {
  val delta = freshVar := deltaangle(tgt, Heading)
  val correctionAngle = freshVar
  val aileronDeflection = freshVar
  val maxBankDeviation = freshVar

  if abs(delta) > maxDeviation then
    if abs(delta) < 10 then
      correctionAngle := lerp(5, 10, 0.5)
      aileronDeflection := 0.1
      maxBankDeviation := 1
    else
      correctionAngle := 60
      aileronDeflection := 0.3
      maxBankDeviation := 5
    bankSet(sign(delta) * correctionAngle, maxBankDeviation, aileronDeflection)
    false
  else
    bankSet(0)
}

@main def HeadingSet = program(testPlane) {
  thrust := 1
  elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.1,0,0.1), 0.1) + Pitch
  headingSet(-60)
}