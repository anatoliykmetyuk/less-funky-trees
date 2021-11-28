package funkycompiler
package examples
package planes

import stage3.*
import stdlib.*

object TestPlane:
  val file = plane("eeePidgin")

  def updateBank(tgt: Tree, maxDeviation: Tree = 5, deflection: Tree = 0.3): Tree = funky {
    if abs(deltaangle(RollAngle, tgt)) > maxDeviation then
      ailerons := sign(RollAngle - tgt) * deflection
    else
      ailerons := smooth(PID(0, RollRate, 0.001, 0, 0), 0.1) + Roll
  }
  def levelPitch: Tree = funky {
    while true do
      thrust := 1
      elevators := smooth(PID(0,PitchAngle+smooth(AngleOfAttack, 0.1),0.01,0.001,0), 0.1) - Pitch
  }
  def setHeading(tgt: Tree, headingSetFlag: Variable = freshVar(),
      maxDeviation: Tree = 0.5, maxCorrectionAngle: Tree = 60): Tree = funky {
    val delta = freshVar()
    val correctionAngle = freshVar()
    val aileronDeflection = freshVar()
    val maxBankDeviation = freshVar()

    while true do
      delta := deltaangle(tgt, Heading)
      if abs(delta) > maxDeviation then
        if abs(delta) < 10 then
          correctionAngle := lerp(5, 10, 0.5)
          aileronDeflection := 0.05
          maxBankDeviation := 1
        else
          correctionAngle := maxCorrectionAngle
          aileronDeflection := 0.1
          maxBankDeviation := 5
        updateBank(sign(delta) * correctionAngle, maxBankDeviation, aileronDeflection)
        headingSetFlag := false
      else
        updateBank(0)
        headingSetFlag := abs(deltaangle(RollAngle, 0)) < maxBankDeviation
  }
